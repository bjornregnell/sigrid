package sigrid.server

import sigrid.common.model.{Room, User}

object Services:

  /** Retrieves all currently active rooms from the database.
    * @return Vector of all Room objects
    */
  def getAllRooms(): Vector[Room] =
    Database.rooms

  /** Logs in a student by creating a user and adding them to a room.
    * Handles room creation if the room doesn't exist.
    * @param name Student's first name
    * @param course Course code
    * @param roomName Room name
    * @return Some((User, Room)) on success, None if room operations fail
    */
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
