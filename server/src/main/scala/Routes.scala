package sigrid.server

import cask.*
import sigrid.common.Serialization.given
import sigrid.common.model.{Role, LoginRequest}
import upickle.default.*

object Routes extends cask.MainRoutes:

  // CORS headers helper
  private val corsHeaders = Seq(
    "Access-Control-Allow-Origin" -> "*",
    "Access-Control-Allow-Methods" -> "GET, POST, PUT, DELETE, OPTIONS",
    "Access-Control-Allow-Headers" -> "Content-Type, Authorization"
  )

  /** Handle CORS preflight requests for /api/login specifically.
    * @return
    *   204 No Content with CORS headers
    */
  @cask.options("/api/login")
  def optionsLogin() =
    cask.Response(data = "", statusCode = 204, headers = corsHeaders)

  /** Health check endpoint.
    * @return
    *   Simple "Pong" response to verify server is running
    */
  @cask.get("/api/ping")
  def ping() =
    cask.Response(data = "Pong", headers = corsHeaders)

  /** Retrieves all active rooms with their students and queues.
    * @return
    *   JSON array of Room objects
    */
  @cask.get("/api/rooms")
  def getRooms() =
    val rooms = Services.getAllRooms()
    cask.Response(data = write(rooms), headers = corsHeaders)

  /** Logs in a user (student or supervisor) and adds them to the specified
    * room. Creates the room if it doesn't exist. Expects JSON body with
    * LoginRequest.
    * @return
    *   JSON tuple of (User, Room) on success, 404 if room creation fails
    */
  @cask.post("/api/login")
  def login(request: cask.Request) =
    val loginRequest = read[LoginRequest](request.text())

    val userRole = loginRequest.role.toLowerCase match
      case "student"    => Role.Student
      case "supervisor" => Role.Supervisor
      case _            => Role.Student

    Services.loginUser(
      loginRequest.name,
      loginRequest.course,
      loginRequest.room,
      userRole
    ) match {
      case Some((user, room)) =>
        cask.Response(data = write((user, room)), headers = corsHeaders)
      case None =>
        cask.Response(
          data = "Room not found",
          statusCode = 404,
          headers = corsHeaders
        )
    }

  initialize()

  println("server started at port=" + port)
  println("debugMode=" + debugMode)
  println("verbose=" + verbose)
