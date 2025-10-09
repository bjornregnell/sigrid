import akka.http.scaladsl.server.StandardRoute
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.model._
import spray.json._
import JsonFormats._

/** API actions that return JSON responses instead of HTML. Reuses the same
  * business logic from db operations.
  */
trait ApiActions {
  self: WebServer =>

  def log(msg: String): Unit = println(s"\nAPI @ ${Date.now().show}> $msg")

  private def replyJson[T: JsonWriter](
      data: T,
      status: StatusCode = StatusCodes.OK
  ): StandardRoute = {
    val json = data.toJson.prettyPrint
    val entity: HttpEntity.Strict =
      HttpEntity(ContentTypes.`application/json`, json)
    complete(status, entity)
  }

  private def errorResponse(error: String, message: String): StandardRoute = {
    log(s"ERROR: $error - $message")
    replyJson(ErrorResponse(error, message), StatusCodes.BadRequest)
  }

  // GET /api/rooms - List all active rooms
  def apiListRooms(): StandardRoute = {
    log("request: GET /api/rooms")
    val nPurgedRooms = db.purgeRemovableRooms()
    if (nPurgedRooms > 0) log(s"purged $nPurgedRooms removable rooms")

    val rooms = db.rooms
    replyJson(RoomListResponse(rooms))
  }

  // GET /api/rooms/{course}/{room} - Get specific room details
  def apiGetRoom(course: String, roomName: String): StandardRoute = {
    log(s"request: GET /api/rooms/$course/$roomName")
    val rk = RoomKey(course, roomName)
    db.roomsToMap.get(rk) match {
      case Some(room) => replyJson(room)
      case None =>
        errorResponse(
          "RoomNotFound",
          s"Room $roomName in course $course does not exist"
        )
    }
  }

  // GET /api/users/{userId} - Get user status and current room
  def apiGetUser(userId: String): StandardRoute = {
    log(s"request: GET /api/users/$userId")
    User.fromUserId(userId) match {
      case Some(user) if db.hasUser(user) =>
        val roomOpt = db.findUserInSomeRoom(user)
        replyJson(UserStatusResponse(user, roomOpt))
      case _ =>
        errorResponse("UserNotFound", s"User $userId does not exist")
    }
  }

  // POST /api/student/login - Student login
  def apiStudentLogin(
      name: String,
      course: String,
      room: String
  ): StandardRoute = {
    log(
      s"request: POST /api/student/login?name=$name&course=$course&room=$room"
    )

    val nPurgedRooms = db.purgeRemovableRooms()
    if (nPurgedRooms > 0) log(s"purged $nPurgedRooms removable rooms")

    val nPurgedUsers = db.purgeRemovableUsers()
    if (nPurgedUsers > 0) log(s"purged $nPurgedUsers removable users")

    val u = db.addUser(name)
    log(s"added $u to userNamesToMap=${db.userNamesToMap}")

    val rOpt = db.addRoomIfNotExists(course, room)
    log(s"room before update: $rOpt")

    val rOpt2 = db.addStudentIfRoomExists(u, course, room)
    log(s"room after updated: $rOpt2")

    rOpt2 match {
      case Some(r) => replyJson(LoginResponse(u, r))
      case None =>
        db.removeUser(u)
        errorResponse(
          "LoginFailed",
          s"Could not add student to room $room in course $course"
        )
    }
  }

  // POST /api/supervisor/login - Supervisor login
  def apiSupervisorLogin(
      name: String,
      course: String,
      room: String
  ): StandardRoute = {
    log(
      s"request: POST /api/supervisor/login?name=$name&course=$course&room=$room"
    )

    val nPurgedRooms = db.purgeRemovableRooms()
    if (nPurgedRooms > 0) log(s"purged $nPurgedRooms removable rooms")

    val nPurgedUsers = db.purgeRemovableUsers()
    if (nPurgedUsers > 0) log(s"purged $nPurgedUsers removable users")

    val u = db.addUser(name)
    log(s"added $u to userNamesToMap=${db.userNamesToMap}")

    val rOpt = db.addRoomIfNotExists(course, room)
    log(s"room before update: $rOpt")

    val rOpt2 = db.addSupervisorIfRoomExists(u, course, room)

    val sup: Set[User] = rOpt2.map(_.supervisors).getOrElse(Set())
    if (sup.contains(u)) {
      log(s"supervisor $u added to room: $rOpt2")
      replyJson(LoginResponse(u, rOpt2.get))
    } else {
      log(s"ERROR: could not add $u to $rOpt2")
      db.removeUser(u)
      errorResponse(
        "LoginFailed",
        s"Could not add supervisor to room $room in course $course"
      )
    }
  }

  // POST /api/student/state - Update student state
  def apiStudentUpdateState(
      userId: String,
      course: String,
      roomCheck: String,
      state: String
  ): StandardRoute = {
    log(
      s"request: POST /api/student/state?userid=$userId&course=$course&room=$roomCheck&state=$state"
    )

    val uOpt = User.fromUserId(userId)
    var movedToRoomMsg = ""

    // Check if room exists, or find user's actual room (in case of room merge)
    val r: String = if (!db.hasRoom(course, roomCheck) && uOpt.isDefined) {
      val rOpt: Option[Room] = db.findUserInSomeRoom(uOpt.get)
      if (rOpt.isDefined) {
        val rn = rOpt.get.name
        if (rn != roomCheck) movedToRoomMsg = s"Moved to room $rn"
        log(s"User $userId is in a non-existing room, moved to $rn")
        rn
      } else roomCheck
    } else roomCheck

    // Validate
    if (!db.hasRoom(course, r)) {
      return errorResponse(
        "RoomNotFound",
        s"Room $r in course $course does not exist"
      )
    }
    if (uOpt.isEmpty || !db.hasUser(uOpt.get)) {
      return errorResponse("UserNotFound", s"User $userId does not exist")
    }
    if (!ui.validStudentState.contains(state)) {
      return errorResponse(
        "InvalidState",
        s"State '$state' is not valid. Valid states: ${ui.validStudentState.mkString(", ")}"
      )
    }

    val user = uOpt.get

    state match {
      case "exit" =>
        log(s"exit student $userId")
        val okOpt = uOpt.map(db.removeUser)
        if (!okOpt.getOrElse(false)) log(s"ERROR: removeUser $userId $uOpt")
        replyJson(
          StateUpdateResponse(
            Room(course, r), // Empty room representation
            s"Student $userId logged out"
          )
        )

      case "help" =>
        val rOpt = db.wantHelp(user, course, r)
        log(s"help $userId updated room to $rOpt")
        rOpt match {
          case Some(room) =>
            replyJson(StateUpdateResponse(room, movedToRoomMsg))
          case None =>
            errorResponse("UpdateFailed", "Could not update to help state")
        }

      case "ready" =>
        val rOpt = db.wantApproval(user, course, r)
        log(s"ready $userId updated room to $rOpt")
        rOpt match {
          case Some(room) =>
            replyJson(StateUpdateResponse(room, movedToRoomMsg))
          case None =>
            errorResponse("UpdateFailed", "Could not update to ready state")
        }

      case "work" =>
        val rOpt = db.working(user, course, r)
        log(s"work $userId updated room to $rOpt")
        rOpt match {
          case Some(room) =>
            replyJson(StateUpdateResponse(room, movedToRoomMsg))
          case None =>
            errorResponse("UpdateFailed", "Could not update to work state")
        }

      case _ =>
        errorResponse("InvalidState", s"Unknown state: $state")
    }
  }

  // POST /api/supervisor/action - Perform supervisor actions
  def apiSupervisorAction(
      userId: String,
      course: String,
      room: String,
      action: String,
      targetUserId: String = "",
      otherRoom: String = ""
  ): StandardRoute = {
    log(
      s"request: POST /api/supervisor/action?userid=$userId&course=$course&room=$room&action=$action"
    )

    // Validate
    if (!db.hasRoom(course, room)) {
      return errorResponse(
        "RoomNotFound",
        s"Room $room in course $course does not exist"
      )
    }
    val uOpt = User.fromUserId(userId)
    if (uOpt.isEmpty || !db.hasUser(uOpt.get)) {
      return errorResponse("UserNotFound", s"User $userId does not exist")
    }
    if (!ui.validSupervisorState.contains(action) && action != "supervising") {
      return errorResponse("InvalidAction", s"Action '$action' is not valid")
    }

    val user = uOpt.get

    action match {
      case "supervising" =>
        log(s"supervising $userId")
        db.roomsToMap.get(RoomKey(course, room)) match {
          case Some(r) => replyJson(StateUpdateResponse(r, ""))
          case None    => errorResponse("RoomNotFound", s"Room not found")
        }

      case "pophelp" =>
        log(s"pophelp chosen by supervisor $userId")
        db.popHelpQueue(course, room) match {
          case Some(r) =>
            log(s"popped help queue in room $course/$room")
            replyJson(StateUpdateResponse(r, "Help queue popped"))
          case None =>
            errorResponse("QueueEmpty", "Help queue is empty or room not found")
        }

      case "popready" =>
        log(s"popready chosen by supervisor $userId")
        db.popApprovalQueue(course, room) match {
          case Some(r) =>
            log(s"popped approval queue in room $course/$room")
            replyJson(StateUpdateResponse(r, "Approval queue popped"))
          case None =>
            errorResponse(
              "QueueEmpty",
              "Approval queue is empty or room not found"
            )
        }

      case "clearhelp" =>
        log(s"clearhelp chosen by supervisor $userId")
        db.clearHelpQueue(course, room) match {
          case Some(r) =>
            log(s"cleared help queue in room $course/$room")
            replyJson(StateUpdateResponse(r, "Help queue cleared"))
          case None =>
            errorResponse("UpdateFailed", "Could not clear help queue")
        }

      case "clearready" =>
        log(s"clearready chosen by supervisor $userId")
        db.clearApprovalQueue(course, room) match {
          case Some(r) =>
            log(s"cleared approval queue in room $course/$room")
            replyJson(StateUpdateResponse(r, "Approval queue cleared"))
          case None =>
            errorResponse("UpdateFailed", "Could not clear approval queue")
        }

      case "removeuser" =>
        log(
          s"action removeuser: supervisor=$userId; try remove name=$targetUserId"
        )
        if (userId == targetUserId) {
          return errorResponse("InvalidOperation", "Cannot remove yourself")
        }
        User.fromUserId(targetUserId) match {
          case Some(targetUser) =>
            if (db.removeUser(targetUser)) {
              log(s"removed user $targetUserId requested by $userId")
              db.roomsToMap.get(RoomKey(course, room)) match {
                case Some(r) =>
                  replyJson(
                    StateUpdateResponse(r, s"User $targetUserId removed")
                  )
                case None =>
                  replyJson(
                    StateUpdateResponse(
                      Room(course, room),
                      s"User $targetUserId removed"
                    )
                  )
              }
            } else {
              errorResponse("UserNotFound", s"User $targetUserId not found")
            }
          case None =>
            errorResponse("InvalidUserId", s"Invalid user ID: $targetUserId")
        }

      case "mergeroom" =>
        log(
          s"action mergeroom: supervisor=$userId; try merge room other=$otherRoom into this room=$room"
        )
        if (!db.hasRoom(course, otherRoom)) {
          return errorResponse(
            "RoomNotFound",
            s"Room $otherRoom does not exist in course $course"
          )
        }
        if (otherRoom == room) {
          return errorResponse(
            "InvalidOperation",
            "Cannot merge room with itself"
          )
        }
        db.mergeRooms(course, otherRoom, room) match {
          case Some(r) =>
            log(s"merged room $otherRoom into $room")
            replyJson(
              StateUpdateResponse(r, s"Room $otherRoom merged into this room")
            )
          case None => errorResponse("MergeFailed", "Could not merge rooms")
        }

      case "gone" =>
        log(s"gone supervisor $userId")
        db.goodbye(user, course, room)
        db.removeUserIfNotInAnyRoom(user)
        replyJson(
          StateUpdateResponse(
            Room(course, room),
            s"Supervisor $userId logged out"
          )
        )

      case "purge" =>
        log(s"purge supervisor $userId room $course $room")
        db.removeRoom(course, room) match {
          case Some(r) =>
            db.removeUserIfNotInAnyRoom(user)
            log(s"removed room $course/$room")
            replyJson(StateUpdateResponse(r, s"Room deleted"))
          case None => errorResponse("RoomNotFound", "Room not found")
        }

      case _ =>
        errorResponse("UnknownAction", s"Unknown action: $action")
    }
  }
}
