object db {
  import scala.util.Try

  val MaxNameLength = 25
  val DefaultEmptyName = "Blanka"

  case class User(name: String, number: Int){
    require(name.nonEmpty, "name must be not be empty")
    require(number > 0, "number $numer must not be less than 1")
    val id = s"${name.toLowerCase}-$number"
  }
  object User {
    def fromString(s: String): Option[User] = Try {
      val xs = s.split('-')
      assert(xs.length == 2)
      val name = xs(0).filter(_.isLetter).take(MaxNameLength).toLowerCase
      User(name,xs(1).toInt)
    }.toOption
  }

  /*private*/ var userNames = new mutable.AtomicDictionary[String, Vector[Int]]

  def userNamesToMap = userNames.toMap

  def users: Set[User] = userNamesToMap.map { 
    case (n, xs) => n -> xs.map(i => User(n, i))
  }.values.flatten.toSet

  /** Create unique User and remember it */
  def addUser(name: String): User = {
    val s = name.filter(_.isLetter).take(MaxNameLength).toLowerCase
    val validName = if (s.isEmpty) DefaultEmptyName else s
    val nextNumbersOpt = userNames.update(validName){ xsOpt => 
      if (xsOpt.isEmpty) Some(Vector(1)) 
      else xsOpt.map(xs => xs :+ (Try(xs.max).getOrElse(0) + 1)) 
    }
    User(validName, nextNumbersOpt.map(_.last).getOrElse(1))
  }

  def hasUser(u: User): Boolean = {
    val ns = userNames.get(u.name).getOrElse(Vector())
    ns.contains(u.number)   
  }

  def removeUser(u: User): Boolean = {
    var existed = false
    userNames.update(u.name){ xsOpt => 
      val n = u.number
      existed = xsOpt.map(_.contains(n)).getOrElse(false)
      val removed = xsOpt.map(xs => xs.filterNot(_ == n)) 
      if (removed == Option(Vector[Int]())) None else removed
    }
    rooms.updateAll((key,room) => room.goodbye(u))
    existed
  }

  def removeAllUsers(): Int = {
    users.foreach(u => rooms.updateAll((key,room) => room.goodbye(u)))
    val n = userNames.size
    userNames.clear()
    n
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

  /* private */ var rooms = new mutable.AtomicDictionary[RoomKey, Room]
  def roomsToMap = rooms.toMap

  def addRoomIfEmpty(
    course: String, 
    roomName: String, 
    supervisorOpt: Option[User]
  ): Option[Room] = {
    rooms.update(RoomKey(course, roomName)){ rOpt =>
      if (rOpt.isEmpty) Option(Room(course, roomName, supervisorOpt)) else rOpt
    }
  }

  def addStudentToRoomIfNonEmpty(
    student: User, 
    course: String, 
    roomName: String
  ): Option[Room] = {
    rooms.update(RoomKey(course, roomName)){ rOpt =>
      if (rOpt.nonEmpty) rOpt.map(r => r.copy(students = r.students + student)) else rOpt
    }
  } 

  def addSupervisorToRoomIfNonEmpty(
    supervisor: User, 
    course: String, 
    roomName: String
  ): Option[Room] = {
    rooms.update(RoomKey(course, roomName)){ rOpt =>
      if (rOpt.nonEmpty) rOpt.map(r => r.copy(supervisorOpt = Some(supervisor))) else rOpt
    }
  } 

  def wantHelp(student: User, course: String, roomName: String): Option[Room] = { 
    val k = RoomKey(course = course, name = roomName)
    rooms.update(k){ rOpt => rOpt.map(_.wantHelp(student)) }
  }

  def wantApproval(u: User, course: String, roomName: String): Option[Room] = {
    val k = RoomKey(course = course, name = roomName)
    rooms.update(k){ rOpt => rOpt.map(_.wantApproval(u)) }
  }

  def working(u: User, course: String, roomName: String): Option[Room] = {
    val k = RoomKey(course = course, name = roomName)
    rooms.update(k){ rOpt => rOpt.map(_.working(u)) }
  }

  def goodbye(u: User, course: String, roomName: String): Option[Room] = {
    val k = RoomKey(course = course, name = roomName)
    rooms.update(k){ rOpt => rOpt.map(_.goodbye(u)) }
  }

}
