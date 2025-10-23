package sigrid.client.views

import com.raquo.laminar.api.L.*
import sigrid.client.network.ApiClient
import sigrid.common.model.{Room, Role}
import sigrid.common.Serialization.given
import scala.concurrent.ExecutionContext.Implicits.global
import sigrid.common.model.User

object Monitor:
  def apply(): HtmlElement =
    val roomsVar = Var[Vector[Room]](Vector.empty)
    val errorVar = Var[Option[String]](None)

    def fetchRooms(): Unit =
      ApiClient
        .get[Vector[Room]]("/rooms")
        .foreach({
          case Right(rooms) =>
            roomsVar.set(rooms)
            errorVar.set(None)
          case Left(error) =>
            roomsVar.set(Vector.empty)
            errorVar.set(Some(s"Kan ej hämta rum från servern: $error"))
        })

    fetchRooms()

    val pollingInterval = 5000

    div(
      className := "monitor",
      h1("Sigrid Monitor"),
      p(
        "Sigrid Monitor visar köer för resurs- och labbtider lajv!"
      ),
      // Error display
      // child <-- errorVar.signal.map({
      //   case Some(error) =>
      //     div(cls := "error-message", error)
      //   case None => emptyNode
      // }),
      mainTag(
        className := "rooms",
        child <-- roomsVar.signal
          .combineWith(errorVar.signal)
          .map({
            case (rooms, error) if rooms.isEmpty && error.isEmpty =>
              div(
                className := "no-rooms-message",
                "Inga aktiva rum för tillfället."
              )
            case _ => emptyNode
          }),
        children <-- roomsVar.signal.map(_.sortBy(_.name).map(renderRoom))
      ),
      onMountCallback { _ =>
        val intervalId = org.scalajs.dom.window.setInterval(
          () => fetchRooms(),
          pollingInterval.toDouble
        )
        // Return cleanup function
        () => { org.scalajs.dom.window.clearInterval(intervalId) }
      }
    )

  private def renderRoom(room: Room): HtmlElement =
    def studentStatus(student: User): String =
      if room.helpQueue.exists(_._1 == student) then "in-help-queue"
      else if room.approvalQueue.exists(_._1 == student) then
        "in-approval-queue"
      else "working"

    def studentWaitTime(student: User): Option[Long] =
      room.helpQueue
        .find(_._1 == student)
        .orElse(room.approvalQueue.find(_._1 == student))
        .map(entry => Room.timeWaitedMinutes(entry))

    val sortedStudents = room.students.toSeq.sortBy(student =>
      studentWaitTime(student) match
        case Some(waitTime) =>
          (0, -waitTime)
        case None => (1, 0L)
    )

    articleTag(
      className := "room",
      headerTag(
        h3(s"${room.name} - ${room.course}")
      ),
      sectionTag(
        className := "student-load",
        sortedStudents.map(student =>
          div(
            className := s"student-box ${studentStatus(student)}",
            title := student.name.capitalize
          )
        )
      ),
      sectionTag(
        className := "supervisors",
        SvgIcon("/icons/supervisor.svg"),
        span(
          if room.supervisors.isEmpty then "Handledare saknas!"
          else room.supervisors.map(_.name.capitalize).mkString(", ")
        )
      ),
      sectionTag(
        className := "queues",
        div(
          className := "queue",
          h4(className := "help-queue-title", "Hjälpkö"),
          Queue(room.helpQueue)
        ),
        div(
          className := "queue",
          h4(className := "approval-queue-title", "Redovisningskö"),
          Queue(room.approvalQueue)
        )
      )
    )
