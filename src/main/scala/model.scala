import java.time.Duration

case class User(name: String, number: Int){
  require(name == User.validName(name), s"invalid user name: $name")
  require(number > 0, s"invalid user number: $number")
  val id = s"$name-$number"
  override def toString = id
}

object User {
  val DefaultUserName = "oddput"
  val MaxNameLength = 25
 
  def validName(s: String): String = 
    if (s.nonEmpty) s.filter(_.isLetter).take(MaxNameLength).toLowerCase 
    else DefaultUserName
  
  def validUserId(s: String): String = 
    s.filter(c => c.isLetterOrDigit || c == '-').toLowerCase

  def fromUserId(uid: String): Option[User] = scala.util.Try {
    val xs = validUserId(uid).split('-')
    assert(xs.length == 2)
    User(validName(xs(0)),xs(1).toInt)
  }.toOption
}

case class RoomKey private (course: String, roomName: String){
  override def toString = s"<br>&nbsp;RoomKey($course,$roomName)"
}
object RoomKey {
  val MaxCourseLength = 25
  val   MaxRoomLength = 20
  val   DefaultCourse = "EDAA45"
  val     DefaultRoom = "Idét"
  val      knownRooms = 
    "Pluto Neptunus Uranus Saturnus Jupiter Mars Venus Elg Elgkalv Hacke Panter Ravel Val Falk Varg Alfa Beta Gamma Idét Distans".split(" ").toSet

  def roomWarning(room: String): String = 
    if (RoomKey.knownRooms.contains(room)) "" 
    else s"""<p class="blink">VARNING: Rum $room okänt i E-huset. Felstavat?</p>""" + 
         s"<p>Kända rum: ${RoomKey.knownRooms.toSeq.sorted.mkString(", ")}.</p>"


  def validCourse(s: String): String = 
    if (s.nonEmpty) s.filter(c => c.isLetterOrDigit).take(MaxCourseLength).toUpperCase 
    else DefaultCourse
  
  def validRoomName(s: String): String = 
    if (s.nonEmpty) s.filter(c => c.isLetterOrDigit).take(MaxRoomLength).toLowerCase.capitalize 
    else DefaultRoom
  
  def apply(course: String, roomName: String): RoomKey = 
    new RoomKey(validCourse(course), validRoomName(roomName))
}

object Room {
  val HoursUntilExpired = 10

  def queueToStringWithTimer(vector: Vector[(User, Date)]): String = {
      def timeWaited(element: (User, Date)): Duration = {
        Duration.between(element._2.dateTime, Date.now().dateTime)
      }

      def durationWaited(element: (User, Date), keepOneDecimal: Boolean = false): String = {
        if (keepOneDecimal) {
          f"${(timeWaited(element).toSeconds()/60.0)}%.1f" // Keeping it here if we want to swich to keeping one decimal.
        } else {
          Math.round(timeWaited(element).toSeconds()/60.0).toString()
        }
      }

      if (vector.size >= 1) {
        val headWaited = f"(<strong>${vector.head._1}</strong>: <small>${durationWaited(vector.head)} min i kö</small>)"
        if (vector.size > 1) {
          headWaited + ", " + vector.tail.map(_._1).mkString(", ")
        } else {
          headWaited
        }
      } else {
        ""
      }
    }
}

case class Room(
  course: String, 
  name: String, 
  supervisors: Set[User] = Set(),
  students: Set[User] = Set(), 
  helpQueue: Vector[(User, Date)] = Vector(), 
  approvalQueue: Vector[(User, Date)] = Vector(), 
  created: Date = Date.now(),
){
  def wantHelp(u: User): Room = copy(
    helpQueue = if (helpQueue.exists(_._1 == u)) helpQueue else helpQueue :+ (u, Date.now()),
    approvalQueue = approvalQueue.filterNot(_._1 == u)
  )

  def wantApproval(u: User): Room = copy(
    helpQueue = helpQueue.filterNot(_._1 == u),
    approvalQueue = if (approvalQueue.exists(_._1 == u)) approvalQueue else approvalQueue :+ (u, Date.now())
  )

  def working(u: User): Room = copy(
    students = students + u,
    helpQueue = helpQueue.filterNot(_._1 == u),
    approvalQueue = approvalQueue.filterNot(_._1 == u)
  )

  def goodbye(u: User): Room = copy(
    students = students - u,
    helpQueue = helpQueue.filterNot(_._1 == u),
    approvalQueue = approvalQueue.filterNot(_._1 == u),
    supervisors = supervisors - u
  )

  def helpQueueString(): String = Room.queueToStringWithTimer(helpQueue)
  def approvalQueueString(): String = Room.queueToStringWithTimer(approvalQueue)

  def clearHelpQueue(): Room = copy(helpQueue = Vector())

  def clearApprovalQueue(): Room = copy(approvalQueue = Vector())

  def popHelpQueue(): Room = copy(helpQueue = helpQueue.drop(1))

  def popApprovalQueue(): Room = copy(approvalQueue = approvalQueue.drop(1))

  def isExpired: Boolean = created < Date.now().minusHours(Room.HoursUntilExpired)

  def isActive: Boolean = supervisors.nonEmpty || students.nonEmpty

  def isRemovable: Boolean = !isActive || isExpired 

  def longestWaitingTimeMinutes: Int = ???

  override def toString = 
    s"Room($course, $name, supervisor=$supervisors, students=$students), helpQueue=$helpQueue, approvalQueue=$approvalQueue, created=${created})"
}
