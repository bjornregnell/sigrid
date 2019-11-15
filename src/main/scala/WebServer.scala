// https://doc.akka.io/docs/akka-http/current/introduction.html
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.StandardRoute
import akka.stream.ActorMaterializer
import scala.io.StdIn

trait WebServer {
  /** Override this with your routes. See doc for akka-http. */
  def routes: Route

  def prompt: String = "server> "
  def getCmd() = Option(StdIn.readLine(prompt)).getOrElse("?").trim.toLowerCase

  def startMsg(host: String, port: Int): String =
    s"Server online at http://$host:$port/\nType ? for help"
  def helpMsg: String = "Type ? for help or stop to exit server."
  def quitMsg: String = "GoodBye!"

  /** Override this with your own  server commands. Return None if unknown. */
  def commandHandler(cmd: String): Option[String] = None

  private def basicCommandHandler(cmd: String): (String, Boolean) = cmd match {
    case "stop"        => "Goodbye!" -> true
    case "help" | "?"  => helpMsg    -> false
    case ""            => ""         -> false
    case otherCommand  => commandHandler(otherCommand)
      .getOrElse(s"Unknown command: $otherCommand\n$helpMsg") -> false
  }

  private def commandLoopUntilQuit(): Unit = {
    var quit = false
    while (!quit) {
      val (msg, isQuitting) = basicCommandHandler(getCmd())
      println(msg)
      quit = isQuitting
    }
  }

  def start(
    args: List[String] = List(),
    port: Int= 8080,
    host: String = "0.0.0.0" //if "localhost" no remote access allowed
    //https://stackoverflow.com/questions/43298909/akka-http-not-allowing-incoming-connections-from-remote-hosts-on-macos
  ): Unit = {
    implicit val system = ActorSystem("my-system")
    implicit val materializer = ActorMaterializer()
    implicit val executionContext = system.dispatcher

    val bindingFuture = Http().bindAndHandle(routes, host, port)
    println(startMsg(host, port))
    commandLoopUntilQuit()

    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ => system.terminate()) // and shutdown when done
  }

  def reply(page: String): StandardRoute = {
    val e: HttpEntity.Strict = HttpEntity(ContentTypes.`text/html(UTF-8)`, page)
    complete(e)
  }

}
