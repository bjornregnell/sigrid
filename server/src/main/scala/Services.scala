package sigrid.server

import sigrid.common.model.{Room, User, Role}

object Services:

  /** Retrieves all currently active rooms from the database.
    * @return
    *   Vector of all Room objects
    */
  def getAllRooms(): Vector[Room] =
    Database.rooms

  /** Logs in a user by creating a user and adding them to a room. Handles room
    * creation if the room doesn't exist.
    * @param name
    *   User's first name
    * @param course
    *   Course code
    * @param roomName
    *   Room name
    * @param role
    *   User role (Student or Supervisor)
    * @return
    *   Some((User, Room)) on success, None if room operations fail
    */
  def loginUser(
      name: String,
      course: String,
      roomName: String,
      role: Role
  ): Option[(User, Room)] =
    // TODO: Find another pattern for database cleanup
    // Purge old data first
    // val nPurgedRooms = Database.purgeRemovableRooms()
    // if (nPurgedRooms > 0) println(s"purged $nPurgedRooms removable rooms")
    // val nPurgedUsers = Database.purgeRemovableUsers()
    // if (nPurgedUsers > 0) println(s"purged $nPurgedUsers removable users")

    val user = Database.addUser(name, role)
    Database.addRoomIfNotExists(course, roomName)

    val roomOpt = role match
      case Role.Student =>
        Database.addStudentIfRoomExists(user, course, roomName)
      case Role.Supervisor =>
        Database.addSupervisorIfRoomExists(user, course, roomName)

    roomOpt.map(room => (user, room))
