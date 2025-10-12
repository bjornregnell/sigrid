package sigrid.server

import scala.util.Try
import storky.Store
import sigrid.common.model.RoomKey
import sigrid.common.model.Room
import sigrid.common.model.User

object Database:
  private val userStore = Store.empty[String, Vector[Int]]()
  private val roomStore = Store.empty[RoomKey, Room]()

  def userNamesToMap: Map[String, Vector[Int]] = userStore.toMap
  def roomsToMap: Map[RoomKey, Room] = roomStore.toMap
  def roomKeys: Set[RoomKey] = roomsToMap.keySet

  def users: Set[User] = userNamesToMap
    .map({ case (userName, userIds) =>
      userName -> userIds.map(userId => User(userName, userId))
    })
    .values
    .flatten
    .toSet

  def rooms: Vector[Room] = roomStore.values.toVector

  def addUser(name: String): User =
    val validName = User.validName(name)
    val updatedUserIds = userStore.update(validName)(existingUserIdsOpt =>
      if existingUserIdsOpt.isEmpty then Some(Vector(1))
      else
        existingUserIdsOpt.map(existingUserIds =>
          existingUserIds :+ (Try(existingUserIds.max).getOrElse(0) + 1)
        )
    )
    User(validName, updatedUserIds.map(_.last).getOrElse(1))

  def hasUser(user: User): Boolean =
    val userIds = userStore.get(user.name).getOrElse(Vector())
    userIds.contains(user.number)

  def hasRoom(course: String, roomName: String): Boolean =
    roomKeys.contains(RoomKey(course, roomName))

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

  def isUserInSomeRoom(user: User): Boolean =
    var found = false
    val roomIterator = roomStore.values.iterator
    while !found && roomIterator.hasNext do
      val room = roomIterator.next()
      if room.students.contains(user) || room.supervisors.contains(user) then
        found = true
    found

  def findUserInSomeRoom(user: User): Option[Room] =
    var found = false
    val roomIterator = roomStore.values.iterator
    var room: Room = null
    while !found && roomIterator.hasNext do
      room = roomIterator.next()
      if room.students.contains(user) || room.supervisors.contains(user) then
        found = true
    if found then Some(room) else None

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

  def purgeRemovableUsers(): Int =
    var removedCount = 0
    users.foreach(user =>
      if removeUserIfNotInAnyRoom(user) then removedCount += 1
    )
    removedCount

  /** Remove room if existing, returns deleted room or None if non-existing. */
  def removeRoom(course: String, roomName: String): Option[Room] =
    val roomKey = RoomKey(course, roomName)
    val removedRoom = roomStore.get(roomKey)
    val usersToMaybeRemove = scala.collection.mutable.ListBuffer.empty[User]
    roomStore.update(roomKey)(roomOpt =>
      roomOpt.foreach(_.students.foreach(user => usersToMaybeRemove += user))
      None
    )
    usersToMaybeRemove.foreach(removeUserIfNotInAnyRoom)
    removedRoom

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

  /** Merge fromRoomName into toRoomName if both exists and delete fromRoomKey,
    * returns merged room if it exists, or None if fromRoomName == toRoomName
    */
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
              students = targetRoom.students ++ sourceRoom.students,
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

  def addStudentIfRoomExists(
      student: User,
      course: String,
      roomName: String
  ): Option[Room] =
    roomStore.update(RoomKey(course, roomName))(roomOpt =>
      roomOpt.map(room => room.copy(students = room.students + student))
    )

  def addSupervisorIfRoomExists(
      supervisor: User,
      course: String,
      roomName: String
  ): Option[Room] =
    roomStore.update(RoomKey(course, roomName))(roomOpt =>
      roomOpt.map(room =>
        room.copy(supervisors = room.supervisors + supervisor)
      )
    )

  def wantHelp(
      student: User,
      course: String,
      roomName: String
  ): Option[Room] =
    val roomKey = RoomKey(course, roomName)
    roomStore.update(roomKey)(roomOpt => roomOpt.map(_.wantHelp(student)))

  def wantApproval(
      student: User,
      course: String,
      roomName: String
  ): Option[Room] =
    val roomKey = RoomKey(course, roomName)
    roomStore.update(roomKey)(roomOpt => roomOpt.map(_.wantApproval(student)))

  def working(user: User, course: String, roomName: String): Option[Room] =
    val roomKey = RoomKey(course, roomName)
    roomStore.update(roomKey)(roomOpt => roomOpt.map(_.working(user)))

  def goodbye(user: User, course: String, roomName: String): Option[Room] =
    val roomKey = RoomKey(course, roomName)
    roomStore.update(roomKey)(roomOpt => roomOpt.map(_.goodbye(user)))

  def popHelpQueue(course: String, roomName: String): Option[Room] =
    val roomKey = RoomKey(course, roomName)
    roomStore.update(roomKey)(roomOpt => roomOpt.map(_.popHelpQueue()))

  def popApprovalQueue(course: String, roomName: String): Option[Room] =
    val roomKey = RoomKey(course, roomName)
    roomStore.update(roomKey)(roomOpt => roomOpt.map(_.popApprovalQueue()))

  def clearHelpQueue(course: String, roomName: String): Option[Room] =
    val roomKey = RoomKey(course, roomName)
    roomStore.update(roomKey)(roomOpt => roomOpt.map(_.clearHelpQueue()))

  def clearApprovalQueue(course: String, roomName: String): Option[Room] =
    val roomKey = RoomKey(course, roomName)
    roomStore.update(roomKey)(roomOpt => roomOpt.map(_.clearApprovalQueue()))
