package sigrid.client.views

import com.raquo.laminar.api.L.*
import sigrid.common.model.{Room, User, Timestamp}

object Queue:
  def apply(queue: Vector[(User, Timestamp)]): HtmlElement =
    div(
      className := "queue-items",
      queue.zipWithIndex.map { case ((user, timestamp), idx) =>
        div(
          span(s"${idx + 1}. ${user.name.capitalize}"),
          span(
            className := "wait-time",
            s"${Room.timeWaitedMinutes((user, timestamp))} min"
          )
        )
      }
    )
