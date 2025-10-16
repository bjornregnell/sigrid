package sigrid.common.model

object Room:
  val HoursUntilExpired = 10
  val MillisPerHour = 60L * 60L * 1000L

  /** Returns current time as Unix timestamp (milliseconds since epoch) */
  def now(): Timestamp = System.currentTimeMillis()

  /** Calculates time waited in milliseconds */
  def timeWaitedMillis(element: (User, Timestamp)): Long =
    now() - element._2

  /** Calculates time waited in minutes */
  def timeWaitedMinutes(element: (User, Timestamp)): Long =
    timeWaitedMillis(element) / (60L * 1000L)

  def queueToStringWithTimer(vector: Vector[(User, Timestamp)]): String =
    def showDurationWaited(
        element: (User, Timestamp),
        keepOneDecimal: Boolean = false
    ): String =
      val minutes = timeWaitedMinutes(element)
      if keepOneDecimal then
        f"${minutes.toDouble}%.1f"
      else minutes.toString

    if vector.size >= 1 then
      val t = showDurationWaited(vector.head)
      val waitMsg = if (t == "0") "" else s"<small> köat $t min</small>"
      val headWaited = f"<strong>${vector.head._1}</strong>$waitMsg"
      if vector.size > 1 then
        headWaited + ", " + vector.tail.map(_._1).mkString(", ")
      else headWaited
    else ""

case class Room(
    course: String,
    name: String,
    supervisors: Set[User] = Set(),
    students: Set[User] = Set(),
    helpQueue: Vector[(User, Timestamp)] = Vector(),
    approvalQueue: Vector[(User, Timestamp)] = Vector(),
    created: Timestamp = Room.now()
):
  def wantHelp(u: User): Room = copy(
    helpQueue =
      if (helpQueue.exists(_._1 == u)) helpQueue
      else helpQueue :+ (u, Room.now()),
    approvalQueue = approvalQueue.filterNot(_._1 == u)
  )

  def wantApproval(u: User): Room = copy(
    helpQueue = helpQueue.filterNot(_._1 == u),
    approvalQueue =
      if (approvalQueue.exists(_._1 == u)) approvalQueue
      else approvalQueue :+ (u, Room.now())
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

  def maxQueuingTime(): Int =
    // Returns the maximum queuing time of both queues as an integer of minutes.
    val queueingTimes = (
      if (helpQueue.size >= 1) Room.timeWaitedMinutes(helpQueue.head).toInt
      else 0,
      if (approvalQueue.size >= 1)
        Room.timeWaitedMinutes(approvalQueue.head).toInt
      else 0
    )
    Math.max(
      queueingTimes._1,
      queueingTimes._2
    )

  def clearHelpQueue(): Room = copy(helpQueue = Vector())

  def clearApprovalQueue(): Room = copy(approvalQueue = Vector())

  def popHelpQueue(): Room = copy(helpQueue = helpQueue.drop(1))

  def popApprovalQueue(): Room = copy(approvalQueue = approvalQueue.drop(1))

  def isExpired: Boolean =
    val expiryTime = created + (Room.HoursUntilExpired * Room.MillisPerHour)
    Room.now() > expiryTime

  def isActive: Boolean = supervisors.nonEmpty || students.nonEmpty

  def isRemovable: Boolean = !isActive || isExpired

  def longestWaitingTimeMinutes: Int = maxQueuingTime()

  override def toString =
    s"Room($course, $name, supervisor=$supervisors, students=$students), helpQueue=$helpQueue, approvalQueue=$approvalQueue, created=${created})"
