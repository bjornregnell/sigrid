package sigrid.server

import cask.*
import sigrid.common.Serialization.given
import upickle.default.*

object Routes extends cask.MainRoutes:

  // CORS headers helper
  private val corsHeaders = Seq(
    "Access-Control-Allow-Origin" -> "*",
    "Access-Control-Allow-Methods" -> "GET, POST, PUT, DELETE, OPTIONS",
    "Access-Control-Allow-Headers" -> "Content-Type, Authorization"
  )

  @cask.get("/api/ping")
  def ping() =
    cask.Response(data = "Pong", headers = corsHeaders)

  @cask.get("/api/rooms")
  def getRooms() =
    val rooms = Services.getAllRooms()
    cask.Response(data = write(rooms), headers = corsHeaders)

  @cask.post("/api/student/login")
  def studentLogin(name: String, course: String, room: String) =
    Services.loginStudent(name, course, room) match {
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
