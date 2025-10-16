package sigrid.server

import cask.*
import sigrid.common.Serialization.given
import sigrid.common.model.Role
import upickle.default.*

object Routes extends cask.MainRoutes:

  // CORS headers helper
  private val corsHeaders = Seq(
    "Access-Control-Allow-Origin" -> "*",
    "Access-Control-Allow-Methods" -> "GET, POST, PUT, DELETE, OPTIONS",
    "Access-Control-Allow-Headers" -> "Content-Type, Authorization"
  )

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
    * room. Creates the room if it doesn't exist.
    * @param name
    *   User's first name
    * @param course
    *   Course code (e.g., "PGK")
    * @param room
    *   Room name (e.g., "Hacke")
    * @param role
    *   Role string ("student" or "supervisor")
    * @return
    *   JSON tuple of (User, Room) on success, 404 if room creation fails
    */
  @cask.post("/api/login")
  def login(name: String, course: String, room: String, role: String) =
    val userRole = role.toLowerCase match
      case "student"    => Role.Student
      case "supervisor" => Role.Supervisor
      case _            => Role.Student

    Services.loginUser(name, course, room, userRole) match {
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
