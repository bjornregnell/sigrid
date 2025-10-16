package sigrid.common

import upickle.default.*
import sigrid.common.model.*

object Serialization:
  // Custom serializer for Date (which wraps LocalDateTime)
  given ReadWriter[Date] = readwriter[String].bimap[Date](
    date => date.show,
    str => {
      import java.time.LocalDateTime
      import java.time.format.DateTimeFormatter
      val dateTime = LocalDateTime.parse(
        str,
        DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
      )
      Date(dateTime)
    }
  )

  // Custom serializer for Role enum (serialize as string)
  given ReadWriter[Role] = readwriter[String].bimap[Role](
    role => role.toString.toLowerCase,
    str =>
      str.toLowerCase match
        case "student"    => Role.Student
        case "supervisor" => Role.Supervisor
        case _ => throw new IllegalArgumentException(s"Invalid role: $str")
  )

  // Auto-derive serializers for case classes
  // User needs to be after Role since it depends on it
  given ReadWriter[User] = macroRW
  given ReadWriter[RoomKey] = macroRW
  given ReadWriter[Room] = macroRW
