package sigrid.server

import sigrid.common.model.{Room, User}

object Services:

  def getAllRooms(): Vector[Room] =
    Database.rooms

  def loginStudent(
      name: String,
      course: String,
      roomName: String
  ): Option[(User, Room)] =
    // TODO: Find another pattern for database cleanup
    // Purge old data first
    // val nPurgedRooms = Database.purgeRemovableRooms()
    // if (nPurgedRooms > 0) println(s"purged $nPurgedRooms removable rooms")
    // val nPurgedUsers = Database.purgeRemovableUsers()
    // if (nPurgedUsers > 0) println(s"purged $nPurgedUsers removable users")

    val user = Database.addUser(name)
    Database.addRoomIfNotExists(course, roomName)
    Database
      .addStudentIfRoomExists(user, course, roomName)
      .map(room => (user, room))
