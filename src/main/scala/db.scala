object db {
  import scala.util.Try

  private var userMap = new mutable.AtomicKeyValueStore[String, Vector[Int]]
  private var roomMap = new mutable.AtomicKeyValueStore[RoomKey, Room]
  
  def userNamesToMap: Map[String, Vector[Int]] = userMap.toMap
  def roomsToMap: Map[RoomKey, Room] = roomMap.toMap
  
  def users: Set[User] = userNamesToMap.map { 
    case (n, xs) => n -> xs.map(i => User(n, i))
  }.values.flatten.toSet

  def rooms: Vector[Room] = roomMap.values.toVector

  /** Create unique User and remember it */
  def addUser(name: String): User = {
    val validName = User.validName(name)
    val nextNumbersOpt = userMap.update(validName){ xsOpt => 
      if (xsOpt.isEmpty) Some(Vector(1)) 
      else xsOpt.map(xs => xs :+ (Try(xs.max).getOrElse(0) + 1)) 
    }
    User(validName, nextNumbersOpt.map(_.last).getOrElse(1))
  }

  def hasUser(u: User): Boolean = {
    val ns = userMap.get(u.name).getOrElse(Vector())
    ns.contains(u.number)   
  }

  def removeUser(u: User): Boolean = {
    var existed = false
    userMap.update(u.name){ xsOpt => 
      val n = u.number
      existed = xsOpt.map(_.contains(n)).getOrElse(false)
      val removed = xsOpt.map(xs => xs.filterNot(_ == n)) 
      if (removed == Option(Vector[Int]())) None else removed
    }
    roomMap.updateAll((key,room) => room.goodbye(u))
    existed
  }

  def removeAllUsers(): Int = {
    users.foreach(u => roomMap.updateAll((key,room) => room.goodbye(u)))
    val n = userMap.size
    userMap.clear()
    n
  }
  
  def addRoomIfEmpty(
    course: String, 
    roomName: String, 
    supervisor: Option[User]
  ): Option[Room] = {
    val rk = RoomKey(course, roomName)
    roomMap.update(rk){ rOpt =>
      if (rOpt.isEmpty) Option(Room(rk.course, rk.roomName, supervisor)) 
      else rOpt
    }
  }

  def addStudentToRoomIfNonEmpty(
    student: User, 
    course: String, 
    roomName: String
  ): Option[Room] = {
    roomMap.update(RoomKey(course, roomName)){ rOpt =>
      if (rOpt.nonEmpty) rOpt.map(r => r.copy(students = r.students + student)) else rOpt
    }
  } 

  def addSupervisorIfNonEmptyRoomAndSupervisorMissing(
    supervisor: User, 
    course: String, 
    roomName: String
  ): Option[Room] = {
    roomMap.update(RoomKey(course, roomName)){ rOpt =>
      if (rOpt.nonEmpty && rOpt.get.supervisor.isEmpty) 
        rOpt.map(r => r.copy(supervisor = Some(supervisor))) 
      else rOpt
    }
  } 

  def wantHelp(student: User, course: String, roomName: String): Option[Room] = {
    val k = RoomKey(course, roomName)
    roomMap.update(k){ rOpt => rOpt.map(_.wantHelp(student)) }
  }

  def wantApproval(student: User, course: String, roomName: String): Option[Room] = {
    val k = RoomKey(course, roomName)
    roomMap.update(k){ rOpt => rOpt.map(_.wantApproval(student)) }
  }

  def working(u: User, course: String, roomName: String): Option[Room] = {
    val k = RoomKey(course, roomName)
    roomMap.update(k){ rOpt => rOpt.map(_.working(u)) }
  }

  def goodbye(u: User, course: String, roomName: String): Option[Room] = {
    val k = RoomKey(course, roomName)
    roomMap.update(k){ rOpt => rOpt.map(_.goodbye(u)) }
  }

}
