object db {
  import scala.util.Try

  /*private*/ var userNames = new mutable.AtomicDictionary[String, Vector[Int]]

  def userNamesToMap = userNames.toMap

  def users: Set[User] = userNamesToMap.map { 
    case (n, xs) => n -> xs.map(i => User(n, i))
  }.values.flatten.toSet

  /** Create unique User and remember it */
  def addUser(name: String): User = {
    val s = name.filter(_.isLetter).take(User.MaxNameLength).toLowerCase
    val validName = if (s.isEmpty) User.DefaultEmptyName else s
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

  def addSupervisorIfNonEmptyRoomAndSupervisorMissing(
    supervisor: User, 
    course: String, 
    roomName: String
  ): Option[Room] = {
    rooms.update(RoomKey(course, roomName)){ rOpt =>
      if (rOpt.nonEmpty && rOpt.get.supervisorOpt.isEmpty) 
        rOpt.map(r => r.copy(supervisorOpt = Some(supervisor))) 
      else rOpt
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
