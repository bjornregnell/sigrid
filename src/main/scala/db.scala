object db {
  import scala.util.Try

  final class AtomicMap[K, V] {
    private val db = new java.util.concurrent.ConcurrentHashMap[K,V]
  
    def put(k: K, v: V): Option[V] = Option(db.put(k, v))
    def get(k: K): Option[V] = if (db.containsKey(k)) Some(db.get(k)) else None
    def remove(k: K): Option[V] = Option(db.remove(k)) 

    /** Update k atomically and return the computed new value. 
     *  The old value Some(v) is given as argument to f 
     *  If value is absent then the argument to f is None. 
     *  If f returns None then the key is removed. */ 
    def update(k: K)(f: Option[V] => Option[V]): Option[V] = {
      import scala.jdk.FunctionConverters._
      val g: (K, V) => V = (k,v) => f(Option(v)).getOrElse(null.asInstanceOf[V])
      Option(db.compute(k, g.asJava))
    }
  
    def toMap: Map[K, V] = {
      import scala.jdk.CollectionConverters._
      db.asScala.toMap
    }

    def size: Int = db.size()

    def clear(): Unit = db.clear()
  
    override def toString: String = toMap.toString
  }
  
  val MaxNameLength = 25
  val DefaultEmptyName = "Blanka"

  case class User(name: String, number: Int){
    require(name.nonEmpty, "name must be not be empty")
    require(number > 0, "number $numer must not be less than 1")
    val show = s"${name.toLowerCase.capitalize}-$number"
  }

  private var users = new AtomicMap[String, Vector[Int]]

  def usersToMap = users.toMap

  /** Create unique User and remember it */
  def addUser(name: String): User = {
    val s = name.filter(_.isLetter).take(MaxNameLength).toLowerCase
    val validName = if (s.isEmpty) DefaultEmptyName else s
    val nextNumbersOpt = users.update(validName){ xsOpt => 
      xsOpt.map(xs => xs :+ (Try(xs.max).getOrElse(0) + 1)) 
    }
    User(validName, nextNumbersOpt.map(_.last).getOrElse(1))
  }

  def hasUser(u: User): Boolean = {
    val ns = users.get(u.name).getOrElse(Vector())
    ns.contains(u.number)   
  }

  def removeUser(u: User): Boolean = {
    var existed = false
    users.update(u.name){ xsOpt => 
      val n = u.number
      existed = xsOpt.map(_.contains(n)).getOrElse(false)
      val removed = xsOpt.map(xs => xs.filterNot(_ == n)) 
      if (removed == Option(Vector[Int]())) None else removed
    }
    existed
  }

  def removeAllUsers(): Int = {
    val n = users.size
    users.clear()
    n
  }

  case class RoomKey(course: String, name: String)

  case class Room(
    course: String, 
    name: String, 
    supervisor: User,
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
      approvalQueue = approvalQueue.filterNot(_ == u)
    )
  }

  private var rooms = new AtomicMap[RoomKey, Room]
  def roomsToMao = rooms.toMap

  def addRoomIfAbsent(course: String, name: String, supervisor: User): Option[Room] = {
    rooms.update(RoomKey(course, name)){ rOpt =>
      if (rOpt.isEmpty) Option(Room(course, name, supervisor)) else rOpt
    }
  }

  def wantHelp(u: User, k: RoomKey): Option[Room] = 
    rooms.update(k){ rOpt => rOpt.map(_.wantHelp(u)) }

  def wantApproval(u: User, k: RoomKey): Option[Room] = 
    rooms.update(k){ rOpt => rOpt.map(_.wantApproval(u)) }

  def goodbye(u: User, k: RoomKey): Option[Room] = 
    rooms.update(k){ rOpt => rOpt.map(_.wantApproval(u)) }

}
