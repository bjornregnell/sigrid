case class User(name: String, number: Int){
  require(name.nonEmpty, "name must be not be empty")
  require(number > 0, "number $numer must not be less than 1")
  val id = s"${name.toLowerCase}-$number"
}
object User {
  val MaxNameLength = 25
  val DefaultEmptyName = "Blanka"
  
  def fromString(s: String): Option[User] = scala.util.Try {
    val xs = s.split('-')
    assert(xs.length == 2)
    val name = xs(0).filter(_.isLetter).take(MaxNameLength).toLowerCase
    User(name,xs(1).toInt)
  }.toOption
}

case class RoomKey(course: String, name: String)

case class Room(
  course: String, 
  name: String, 
  supervisorOpt: Option[User],
  students: Set[User] = Set(), 
  helpQueue: Vector[User] = Vector(), 
  approvalQueue: Vector[User] = Vector(), 
){
  def wantHelp(u: User): Room = copy(
    helpQueue = helpQueue.filterNot(_ == u) :+ u,
    approvalQueue = approvalQueue.filterNot(_ == u)
  )

  def wantApproval(u: User): Room = copy(
    helpQueue = helpQueue.filterNot(_ == u),
    approvalQueue = approvalQueue.filterNot(_ == u) :+ u
  )

  def working(u: User): Room = copy(
    students = students + u,
    helpQueue = helpQueue.filterNot(_ == u),
    approvalQueue = approvalQueue.filterNot(_ == u)
  )

  def goodbye(u: User): Room = copy(
    students = students - u,
    helpQueue = helpQueue.filterNot(_ == u),
    approvalQueue = approvalQueue.filterNot(_ == u),
    supervisorOpt = supervisorOpt.flatMap(s => if (s == u) None else Some(s))
  )
}