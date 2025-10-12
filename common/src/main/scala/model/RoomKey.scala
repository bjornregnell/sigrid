package sigrid.common.model

case class RoomKey private (course: String, roomName: String):
  override def toString = s"<br>&nbsp;RoomKey($course,$roomName)"

object RoomKey:
  val MaxCourseLength = 25
  val MaxRoomLength = 20
  val DefaultCourse = "pgk"
  val DefaultRoom = "Idét"
  val knownRooms =
    "Pluto Neptunus Uranus Saturnus Jupiter Mars Venus Elg Elgkalv Hacke Panter Ravel Val Falk Varg Lo Alfa Beta Gamma Idét Distans"
      .split(" ")
      .toSet

  // THIS IS A HACK, same thing different name, see https://kurser.lth.se/lot/course-syllabus/23_24/EITA65
  val fromCourseCodeToCourseName =
    Map(
      "EDAA45" -> "PGK",
      "EDAB05" -> "PGK",
      "EDAA60" -> "DOD",
      "EITA65" -> "DOD"
    )

  def validCourse(s: String): String =
    if (s.nonEmpty) {
      val course =
        s.filter(c => c.isLetterOrDigit).take(MaxCourseLength).toUpperCase
      fromCourseCodeToCourseName.getOrElse(course.take(6), course)
    } else DefaultCourse

  def validRoomName(s: String): String =
    if (s.nonEmpty)
      s.filter(c => c.isLetterOrDigit)
        .take(MaxRoomLength)
        .toLowerCase
        .capitalize
    else DefaultRoom

  def apply(course: String, roomName: String): RoomKey =
    new RoomKey(validCourse(course), validRoomName(roomName))