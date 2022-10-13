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
      roomStore.updateAll((key,room) => room.goodbye(u))
      val removed = xsOpt.map(xs => xs.filterNot(_ == n)) 
      if (removed == Option(Vector[Int]())) None else removed
    }
    existed
  }

  def removeUserIfNotInAnyRoom(u: User): Boolean = {
    var wasRemoved = false
    userStore.update(u.name){ xsOpt => 
      val n = u.number
      val existed = xsOpt.map(_.contains(n)).getOrElse(false)
      if (!isUserInSomeRoom(u)) {
        roomStore.updateAll((key,room) => room.goodbye(u))
        val removed = xsOpt.map(xs => xs.filterNot(_ == n))
        wasRemoved = existed
        if (removed == Option(Vector[Int]())) None else removed
      } else xsOpt
    }
    wasRemoved
  }


  def isUserInSomeRoom(u: User): Boolean = {
    var found = false
    val it = roomStore.values.iterator
    while (!found && it.hasNext) {
      val r = it.next()
      if (r.students.contains(u) || r.supervisors.contains(u)) found = true      
    }
    found
  }

  def findUserInSomeRoom(u: User): Option[Room] = {
    var found = false
    val it = roomStore.values.iterator
    var r: Room = null
    while (!found && it.hasNext) {
      r = it.next()
      if (r.students.contains(u) || r.supervisors.contains(u)) found = true      
    }
    if (found) Some(r) else None
  }


  def purgeRemovableRooms(): Int = {  // TODO: investigate if this is thread safe ???
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

  def purgeRemovableUsers(): Int = {    
    var n = 0
    users.foreach { u => 
        if (removeUserIfNotInAnyRoom(u))  n += 1
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
    usersToMaybeRemove.foreach(removeUserIfNotInAnyRoom)
    rDel
  }

  def addRoomIfNotExists(
    course: String, 
    roomName: String, 
  ): Option[Room] = {
    val rk = RoomKey(course, roomName)
    roomStore.update(rk){ rOpt =>
      if (rOpt.isEmpty) Option(Room(course = rk.course, name = rk.roomName)) 
      else rOpt
    }
  }

  /** Merge fromRoomName into toRoomName if both exists and delete fromRoomKey,
    * returns merged room if it exists, or None if fromRoomName == toRoomName*/
  def mergeRooms(
    course: String, 
    fromRoomName: String, 
    toRoomName: String, 
  ): Option[Room] = {
    if (fromRoomName != toRoomName) { // must be different
      val fromRoomKey = RoomKey(course, fromRoomName)
      val toRoomKey   = RoomKey(course, toRoomName)
      roomStore.update(toRoomKey){ toOpt =>
        toOpt.map { t =>
          val fromOpt: Option[Room] = roomStore.get(fromRoomKey) 
          if (fromOpt.isDefined) {
            val f = fromOpt.get
            val updatedRoom = t.copy(
              students = t.students ++ f.students, 
              helpQueue = t.helpQueue ++ f.helpQueue, 
              approvalQueue = t.approvalQueue ++ f.approvalQueue
            )
            roomStore.remove(fromRoomKey)
            updatedRoom
          } else t  // don't change anything if fromRoom does not exist 
        }
      }
    } else None
  }

  def addStudentIfRoomExists(
    student: User, 
    course: String, 
    roomName: String
  ): Option[Room] = {
    roomStore.update(RoomKey(course, roomName)){ rOpt =>
      rOpt.map(r => r.copy(students = r.students + student)) 
    }
  } 

  def addSupervisorIfRoomExists(
    supervisor: User, 
    course: String, 
    roomName: String
  ): Option[Room] = {
    roomStore.update(RoomKey(course, roomName)){ rOpt =>
        rOpt.map(r => r.copy(supervisors = r.supervisors + supervisor)) 
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
    roomStore.update(k){ rOpt => rOpt.map(_.popHelpQueue()) }
  }

  def popApprovalQueue(course: String, roomName: String): Option[Room] = {
    val k = RoomKey(course, roomName)
    roomStore.update(k){ rOpt => rOpt.map(_.popApprovalQueue()) }
  }

  def clearHelpQueue(course: String, roomName: String): Option[Room] = {
    val k = RoomKey(course, roomName)
    roomStore.update(k){ rOpt => rOpt.map(_.clearHelpQueue()) }
  }

  def clearApprovalQueue(course: String, roomName: String): Option[Room] = {
    val k = RoomKey(course, roomName)
    roomStore.update(k){ rOpt => rOpt.map(_.clearApprovalQueue()) }
  }

}
