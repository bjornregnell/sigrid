object db {
  import scala.util.Try

  private var userStore = new mutable.AtomicKeyValueStore[String, Vector[Int]]
  private var roomStore = new mutable.AtomicKeyValueStore[RoomKey, Room]
  
  def userNamesToMap: Map[String, Vector[Int]] = userStore.toMap
  def roomsToMap: Map[RoomKey, Room] = roomStore.toMap
  def roomKeys: Set[RoomKey] = roomsToMap.keySet
  
  def users: Set[User] = userNamesToMap.map { 
    case (n, xs) => n -> xs.map(i => User(n, i))
  }.values.flatten.toSet

  def rooms: Vector[Room] = roomStore.values.toVector

  /** Create unique User and remember it */
  def addUser(name: String): User = {
    val validName = User.validName(name)
    val nextNumbersOpt = userStore.update(validName){ xsOpt => 
      if (xsOpt.isEmpty) Some(Vector(1)) 
      else xsOpt.map(xs => xs :+ (Try(xs.max).getOrElse(0) + 1)) 
    }
    User(validName, nextNumbersOpt.map(_.last).getOrElse(1))
  }

  def hasUser(u: User): Boolean = {
    val ns = userStore.get(u.name).getOrElse(Vector())
    ns.contains(u.number)   
  }

  def hasRoom(course: String, roomName: String): Boolean = 
    roomKeys.contains(RoomKey(course, roomName))

  def removeUser(u: User): Boolean = {
    var existed = false
    userStore.update(u.name){ xsOpt => 
      val n = u.number
      existed = xsOpt.map(_.contains(n)).getOrElse(false)
      val removed = xsOpt.map(xs => xs.filterNot(_ == n)) 
      if (removed == Option(Vector[Int]())) None else removed
    }
    roomStore.updateAll((key,room) => room.goodbye(u))
    existed
  }

  def isUserInSomeRoom(u: User): Boolean = {
    var found = false
    val it = roomStore.values.iterator
    while (!found && it.hasNext) {
      if (it.next.students.contains(u)) found = true      
    }
    found
  }

  def removeAllUsers(): Int = {
    users.foreach(u => roomStore.updateAll((key,room) => room.goodbye(u)))
    val n = userStore.size
    userStore.clear()
    n
  }

  def purgeRemovableRooms(): Int = {  // TODO test this
    var n = 0
    roomKeys.foreach { rk =>
      roomStore.update(rk){ rOpt => 
        rOpt.flatMap { r =>
          if (r.isRemovable) { 
            n += 1
            None 
          } else Some(r)
        }
      }
    }
    n
 }

  /** Remove room if existing, returns deleted room or None if non-existing.*/
  def removeRoom(course: String, roomName: String): Option[Room] = {
    val rk = RoomKey(course, roomName)
    val rDel = roomStore.get(rk) 
    val usersToMaybeRemove = scala.collection.mutable.ListBuffer.empty[User]
    roomStore.update(rk){ rOpt => 
      rOpt.foreach(_.students.foreach(u => usersToMaybeRemove += u))
      None 
    }
    usersToMaybeRemove.foreach(u => if (!isUserInSomeRoom(u)) removeUser(u))
    rDel
  }

  def addRoomIfEmpty(
    course: String, 
    roomName: String, 
    supervisor: Option[User]
  ): Option[Room] = {
    val rk = RoomKey(course, roomName)
    roomStore.update(rk){ rOpt =>
      if (rOpt.isEmpty) Option(Room(rk.course, rk.roomName, supervisor)) 
      else rOpt
    }
  }

  def addStudentToRoomIfNonEmpty(
    student: User, 
    course: String, 
    roomName: String
  ): Option[Room] = {
    roomStore.update(RoomKey(course, roomName)){ rOpt =>
      if (rOpt.nonEmpty) rOpt.map(r => r.copy(students = r.students + student)) else rOpt
    }
  } 

  def addSupervisorIfNonEmptyRoomAndSupervisorMissing(
    supervisor: User, 
    course: String, 
    roomName: String
  ): Option[Room] = {
    roomStore.update(RoomKey(course, roomName)){ rOpt =>
      if (rOpt.nonEmpty && rOpt.get.supervisor.isEmpty) 
        rOpt.map(r => r.copy(supervisor = Some(supervisor))) 
      else rOpt
    }
  } 

  def wantHelp(student: User, course: String, roomName: String): Option[Room] = {
    val k = RoomKey(course, roomName)
    roomStore.update(k){ rOpt => rOpt.map(_.wantHelp(student)) }
  }

  def wantApproval(student: User, course: String, roomName: String): Option[Room] = {
    val k = RoomKey(course, roomName)
    roomStore.update(k){ rOpt => rOpt.map(_.wantApproval(student)) }
  }

  def working(u: User, course: String, roomName: String): Option[Room] = {
    val k = RoomKey(course, roomName)
    roomStore.update(k){ rOpt => rOpt.map(_.working(u)) }
  }

  def goodbye(u: User, course: String, roomName: String): Option[Room] = {
    val k = RoomKey(course, roomName)
    roomStore.update(k){ rOpt => rOpt.map(_.goodbye(u)) }
  }

  def popHelpQueue(course: String, roomName: String): Option[Room] = {
    val k = RoomKey(course, roomName)
    roomStore.update(k){ rOpt => rOpt.map(_.clearHelpQueue()) }
  }

  def popApprovalQueue(course: String, roomName: String): Option[Room] = {
    val k = RoomKey(course, roomName)
    roomStore.update(k){ rOpt => rOpt.map(_.popApprovalQueue()) }
  }

  def clearHelpQueue(course: String, roomName: String): Option[Room] = {
    val k = RoomKey(course, roomName)
    roomStore.update(k){ rOpt => rOpt.map(_.popHelpQueue()) }
  }

  def clearApprovalQueue(course: String, roomName: String): Option[Room] = {
    val k = RoomKey(course, roomName)
    roomStore.update(k){ rOpt => rOpt.map(_.clearApprovalQueue()) }
  }

}
