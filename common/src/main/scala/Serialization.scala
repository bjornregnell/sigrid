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

  // Auto-derive serializers for case classes
  given ReadWriter[User] = macroRW
  given ReadWriter[RoomKey] = macroRW
  given ReadWriter[Room] = macroRW
