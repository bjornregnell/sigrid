import spray.json._

/** JSON serialization formats for API responses */
object JsonFormats extends DefaultJsonProtocol {

  // Basic formats for User
  implicit object UserFormat extends RootJsonFormat[User] {
    def write(u: User) = JsObject(
      "name" -> JsString(u.name),
      "number" -> JsNumber(u.number),
      "id" -> JsString(u.id)
    )

    def read(value: JsValue) =
      value.asJsObject.getFields("name", "number") match {
        case Seq(JsString(name), JsNumber(number)) => User(name, number.toInt)
        case _ => throw DeserializationException("User expected")
      }
  }

  // Format for Date
  implicit object DateFormat extends RootJsonFormat[Date] {
    def write(d: Date) = JsObject(
      "year" -> JsNumber(d.year),
      "month" -> JsNumber(d.month),
      "day" -> JsNumber(d.dayOfMonth),
      "hour" -> JsNumber(d.hour),
      "minute" -> JsNumber(d.minute),
      "second" -> JsNumber(d.second),
      "display" -> JsString(d.show)
    )

    def read(value: JsValue) = {
      val fields = value.asJsObject.fields
      Date(
        year = fields("year").convertTo[Int],
        month = fields("month").convertTo[Int],
        dayOfMonth = fields("day").convertTo[Int],
        hour = fields("hour").convertTo[Int],
        minute = fields("minute").convertTo[Int],
        second = fields("second").convertTo[Int]
      )
    }
  }

  // Format for RoomKey
  implicit object RoomKeyFormat extends RootJsonFormat[RoomKey] {
    def write(rk: RoomKey) = JsObject(
      "course" -> JsString(rk.course),
      "roomName" -> JsString(rk.roomName)
    )

    def read(value: JsValue) =
      value.asJsObject.getFields("course", "roomName") match {
        case Seq(JsString(course), JsString(roomName)) =>
          RoomKey(course, roomName)
        case _ => throw DeserializationException("RoomKey expected")
      }
  }

  // Format for queue items (User, Date) tuples
  implicit object QueueItemFormat extends RootJsonFormat[(User, Date)] {
    def write(item: (User, Date)) = JsObject(
      "user" -> item._1.toJson,
      "timestamp" -> item._2.toJson,
      "minutesWaited" -> JsNumber(Room.timeWaited(item).toMinutes)
    )

    def read(value: JsValue) =
      value.asJsObject.getFields("user", "timestamp") match {
        case Seq(user, timestamp) =>
          (user.convertTo[User], timestamp.convertTo[Date])
        case _ => throw DeserializationException("QueueItem expected")
      }
  }

  // Format for Room
  implicit object RoomFormat extends RootJsonFormat[Room] {
    def write(r: Room) = JsObject(
      "course" -> JsString(r.course),
      "name" -> JsString(r.name),
      "supervisors" -> JsArray(r.supervisors.map(_.toJson).toVector),
      "students" -> JsArray(r.students.map(_.toJson).toVector),
      "helpQueue" -> JsArray(r.helpQueue.map(_.toJson)),
      "approvalQueue" -> JsArray(r.approvalQueue.map(_.toJson)),
      "created" -> r.created.toJson,
      "isActive" -> JsBoolean(r.isActive),
      "isExpired" -> JsBoolean(r.isExpired),
      "maxQueuingTime" -> JsNumber(r.maxQueuingTime())
    )

    def read(value: JsValue) = {
      val fields = value.asJsObject.fields
      Room(
        course = fields("course").convertTo[String],
        name = fields("name").convertTo[String],
        supervisors = fields("supervisors").convertTo[Vector[User]].toSet,
        students = fields("students").convertTo[Vector[User]].toSet,
        helpQueue = fields("helpQueue").convertTo[Vector[(User, Date)]],
        approvalQueue = fields("approvalQueue").convertTo[Vector[(User, Date)]],
        created = fields("created").convertTo[Date]
      )
    }
  }

  // API response formats
  case class ErrorResponse(error: String, message: String)
  implicit val errorResponseFormat: RootJsonFormat[ErrorResponse] = jsonFormat2(
    ErrorResponse
  )

  case class LoginResponse(user: User, room: Room)
  implicit val loginResponseFormat: RootJsonFormat[LoginResponse] = jsonFormat2(
    LoginResponse
  )

  case class StateUpdateResponse(room: Room, message: String)
  implicit val stateUpdateResponseFormat: RootJsonFormat[StateUpdateResponse] =
    jsonFormat2(StateUpdateResponse)

  case class RoomListResponse(rooms: Vector[Room])
  implicit val roomListResponseFormat: RootJsonFormat[RoomListResponse] =
    jsonFormat1(RoomListResponse)

  case class UserStatusResponse(user: User, room: Option[Room])
  implicit val userStatusResponseFormat: RootJsonFormat[UserStatusResponse] =
    jsonFormat2(UserStatusResponse)
}
