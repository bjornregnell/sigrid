import cask.*
import sigrid.common.Utils

object Main extends cask.MainRoutes:

  @cask.get("/ping")
  def ping() =
    cask.Response(
      "Pong",
      // Temporary CORS-headers (https://developer.mozilla.org/en-US/docs/Web/HTTP/Guides/CORS)
      headers = Seq(
        "Access-Control-Allow-Origin" -> "*",
        "Access-Control-Allow-Methods" -> "GET, POST, PUT, DELETE, OPTIONS",
        "Access-Control-Allow-Headers" -> "Content-Type, Authorization"
      )
    )

  initialize()
