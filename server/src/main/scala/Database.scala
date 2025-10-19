package sigrid.server

import scala.util.Try
import storky.Store
import sigrid.common.model.{RoomKey, Room, User, Role}

object Database:
  private val userStore = Store.empty[String, Vector[Int]]()
  private val roomStore = Store.empty[RoomKey, Room]()

  /** Gets mapping of user names to their assigned numbers. */
  def userNamesToMap: Map[String, Vector[Int]] = userStore.toMap

  /** Gets mapping of room keys to room objects. */
  def roomsToMap: Map[RoomKey, Room] = roomStore.toMap

  /** Gets all room keys in the database. */
  def roomKeys: Set[RoomKey] = roomsToMap.keySet

  /** Gets all users across all rooms. */
  def users: Set[User] =
    // Collect users from actual rooms since we need their role
    rooms.flatMap(room => room.users).toSet

  /** Gets all active rooms. */
  def rooms: Vector[Room] = roomStore.values.toVector

  /** Creates a new user with auto-incremented number. Handles name validation
    * and number assignment.
    * @param name
    *   Raw user name input
    * @param role
    *   User role (Student or Supervisor)
    * @return
    *   New User with validated name, unique number, and role
    */
  def addUser(name: String, role: Role): User =
    val validName = User.validName(name)
    val updatedUserIds = userStore.update(validName)(existingUserIdsOpt =>
      if existingUserIdsOpt.isEmpty then Some(Vector(1))
      else
        existingUserIdsOpt.map(existingUserIds =>
          existingUserIds :+ (Try(existingUserIds.max).getOrElse(0) + 1)
        )
    )
    User(validName, updatedUserIds.map(_.last).getOrElse(1), role)

  /** Checks if a user exists in the database. */
  def hasUser(user: User): Boolean =
    val userIds = userStore.get(user.name).getOrElse(Vector())
    userIds.contains(user.number)

  /** Checks if a room exists for the given course and name. */
  def hasRoom(course: String, roomName: String): Boolean =
    roomKeys.contains(RoomKey(course, roomName))

  /** Removes a user from the database and all rooms. */
  def removeUser(user: User): Boolean =
    var existed = false
    userStore.update(user.name)(existingUserIdsOpt =>
      val userId = user.number
      existed = existingUserIdsOpt.map(_.contains(userId)).getOrElse(false)
      roomStore.updateAll((key, room) => room.goodbye(user))
      val filteredIds =
        existingUserIdsOpt.map(userIds => userIds.filterNot(_ == userId))
      if filteredIds == Option(Vector[Int]()) then None else filteredIds
    )
    existed

  /** Removes a user only if they're not in any room. */
  def removeUserIfNotInAnyRoom(user: User): Boolean =
    var wasRemoved = false
    userStore.update(user.name)(existingUserIdsOpt =>
      val userId = user.number
      val existed = existingUserIdsOpt.map(_.contains(userId)).getOrElse(false)
      if !isUserInSomeRoom(user) then
        roomStore.updateAll((key, room) => room.goodbye(user))
        val filteredIds =
          existingUserIdsOpt.map(userIds => userIds.filterNot(_ == userId))
        wasRemoved = existed
        if filteredIds == Option(Vector[Int]()) then None else filteredIds
      else existingUserIdsOpt
    )
    wasRemoved

  /** Checks if a user is currently in any room. */
  def isUserInSomeRoom(user: User): Boolean =
    var found = false
    val roomIterator = roomStore.values.iterator
    while !found && roomIterator.hasNext do
      val room = roomIterator.next()
      if room.users.contains(user) then
        found = true
    found

  /** Finds the room containing the specified user. */
  def findUserInSomeRoom(user: User): Option[Room] =
    var found = false
    val roomIterator = roomStore.values.iterator
    var room: Room = null
    while !found && roomIterator.hasNext do
      room = roomIterator.next()
      if room.users.contains(user) then
        found = true
    if found then Some(room) else None

  /** Removes all rooms marked as removable. */
  def purgeRemovableRooms()
      : Int = // TODO: investigate if this is thread safe ???
    var removedCount = 0
    roomKeys.foreach(roomKey =>
      roomStore.update(roomKey)(roomOpt =>
        roomOpt.flatMap(room =>
          if room.isRemovable then
            removedCount += 1
            None
          else Some(room)
        )
      )
    )
    removedCount

  /** Removes all users not currently in any room. */
  def purgeRemovableUsers(): Int =
    var removedCount = 0
    users.foreach(user =>
      if removeUserIfNotInAnyRoom(user) then removedCount += 1
    )
    removedCount

  /** Removes a room and cleans up orphaned users. */
  def removeRoom(course: String, roomName: String): Option[Room] =
    val roomKey = RoomKey(course, roomName)
    val removedRoom = roomStore.get(roomKey)
    val usersToMaybeRemove = scala.collection.mutable.ListBuffer.empty[User]
    roomStore.update(roomKey)(roomOpt =>
      roomOpt.foreach(_.users.foreach(user => usersToMaybeRemove += user))
      None
    )
    usersToMaybeRemove.foreach(removeUserIfNotInAnyRoom)
    removedRoom

  /** Creates a new room if it doesn't already exist.
    * @param course
    *   Course code
    * @param roomName
    *   Room name
    * @return
    *   Some(Room) if room exists or was created, None if invalid
    */
  def addRoomIfNotExists(
      course: String,
      roomName: String
  ): Option[Room] =
    val roomKey = RoomKey(course, roomName)
    roomStore.update(roomKey)(roomOpt =>
      if roomOpt.isEmpty then
        Option(Room(course = roomKey.course, name = roomKey.roomName))
      else roomOpt
    )

  /** Merges two rooms by combining their users and queues. */
  def mergeRooms(
      course: String,
      fromRoomName: String,
      toRoomName: String
  ): Option[Room] =
    if fromRoomName != toRoomName then // must be different
      val fromRoomKey = RoomKey(course, fromRoomName)
      val toRoomKey = RoomKey(course, toRoomName)
      roomStore.update(toRoomKey)(toRoomOpt =>
        toRoomOpt.map(targetRoom =>
          val fromRoomOpt: Option[Room] = roomStore.get(fromRoomKey)
          if fromRoomOpt.isDefined then
            val sourceRoom = fromRoomOpt.get
            val updatedRoom = targetRoom.copy(
              users = targetRoom.users ++ sourceRoom.users,
              helpQueue = targetRoom.helpQueue ++ sourceRoom.helpQueue,
              approvalQueue =
                targetRoom.approvalQueue ++ sourceRoom.approvalQueue
            )
            roomStore.remove(fromRoomKey)
            updatedRoom
          else targetRoom // don't change anything if fromRoom does not exist
        )
      )
    else None

  /** Adds a user to an existing room.
    * @param user
    *   The User to add (can be student or supervisor)
    * @param course
    *   Course code
    * @param roomName
    *   Room name
    * @return
    *   Some(updated Room) if room exists, None otherwise
    */
  def addUserIfRoomExists(
      user: User,
      course: String,
      roomName: String
  ): Option[Room] =
    roomStore.update(RoomKey(course, roomName))(roomOpt =>
      roomOpt.map(room => room.copy(users = room.users + user))
    )

  /** Adds a student to the help queue. */
  def wantHelp(
      student: User,
      course: String,
      roomName: String
  ): Option[Room] =
    val roomKey = RoomKey(course, roomName)
    roomStore.update(roomKey)(roomOpt => roomOpt.map(_.wantHelp(student)))

  /** Adds a student to the approval queue. */
  def wantApproval(
      student: User,
      course: String,
      roomName: String
  ): Option[Room] =
    val roomKey = RoomKey(course, roomName)
    roomStore.update(roomKey)(roomOpt => roomOpt.map(_.wantApproval(student)))

  /** Removes a user from all queues (they're now working). */
  def working(user: User, course: String, roomName: String): Option[Room] =
    val roomKey = RoomKey(course, roomName)
    roomStore.update(roomKey)(roomOpt => roomOpt.map(_.working(user)))

  /** Removes a user from all queues and room. */
  def goodbye(user: User, course: String, roomName: String): Option[Room] =
    val roomKey = RoomKey(course, roomName)
    roomStore.update(roomKey)(roomOpt => roomOpt.map(_.goodbye(user)))

  /** Removes the first student from the help queue. */
  def popHelpQueue(course: String, roomName: String): Option[Room] =
    val roomKey = RoomKey(course, roomName)
    roomStore.update(roomKey)(roomOpt => roomOpt.map(_.popHelpQueue()))

  /** Removes the first student from the approval queue. */
  def popApprovalQueue(course: String, roomName: String): Option[Room] =
    val roomKey = RoomKey(course, roomName)
    roomStore.update(roomKey)(roomOpt => roomOpt.map(_.popApprovalQueue()))

  /** Clears all students from the help queue. */
  def clearHelpQueue(course: String, roomName: String): Option[Room] =
    val roomKey = RoomKey(course, roomName)
    roomStore.update(roomKey)(roomOpt => roomOpt.map(_.clearHelpQueue()))

  /** Clears all students from the approval queue. */
  def clearApprovalQueue(course: String, roomName: String): Option[Room] =
    val roomKey = RoomKey(course, roomName)
    roomStore.update(roomKey)(roomOpt => roomOpt.map(_.clearApprovalQueue()))
