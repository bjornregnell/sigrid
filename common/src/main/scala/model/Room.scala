package sigrid.common.model

import java.time.Duration

object Room:
  val HoursUntilExpired = 10

  def timeWaited(element: (User, Date)): Duration =
    Duration.between(element._2.dateTime, Date.now().dateTime)

  def queueToStringWithTimer(vector: Vector[(User, Date)]): String =
    def showDurationWaited(
        element: (User, Date),
        keepOneDecimal: Boolean = false
    ): String =
      if keepOneDecimal then
        f"${(timeWaited(element).toSeconds() / 60.0)}%.1f" // Keeping it here if we want to switch to keeping one decimal.
      else Math.round(timeWaited(element).toSeconds() / 60.0).toString()

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
    helpQueue: Vector[(User, Date)] = Vector(),
    approvalQueue: Vector[(User, Date)] = Vector(),
    created: Date = Date.now()
):
  def wantHelp(u: User): Room = copy(
    helpQueue =
      if (helpQueue.exists(_._1 == u)) helpQueue
      else helpQueue :+ (u, Date.now()),
    approvalQueue = approvalQueue.filterNot(_._1 == u)
  )

  def wantApproval(u: User): Room = copy(
    helpQueue = helpQueue.filterNot(_._1 == u),
    approvalQueue =
      if (approvalQueue.exists(_._1 == u)) approvalQueue
      else approvalQueue :+ (u, Date.now())
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
      if (helpQueue.size >= 1) Room.timeWaited(helpQueue.head).toMinutes().toInt
      else 0,
      if (approvalQueue.size >= 1)
        Room.timeWaited(approvalQueue.head).toMinutes().toInt
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
    created < Date.now().minusHours(Room.HoursUntilExpired)

  def isActive: Boolean = supervisors.nonEmpty || students.nonEmpty

  def isRemovable: Boolean = !isActive || isExpired

  def longestWaitingTimeMinutes: Int = ???

  override def toString =
    s"Room($course, $name, supervisor=$supervisors, students=$students), helpQueue=$helpQueue, approvalQueue=$approvalQueue, created=${created})"
