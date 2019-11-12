// https://doc.akka.io/docs/akka-http/current/introduction.html
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import scala.io.StdIn
import akka.http.scaladsl.server.StandardRoute
import db.Room

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

  def reply(body: String): StandardRoute = {
    val e: HttpEntity.Strict = HttpEntity(ContentTypes.`text/html(UTF-8)`, body)
    complete(e)
  }

  def log(msg: String): Unit = println(s"\n${new java.util.Date}: $msg")
}

object SigridServer extends WebServer {
  override def routes =
    path("hello") { get {  
      log(s"request: /hello")
      reply(ui.helloPage) 
    } }  ~
    path("beppe") { get {
      log(s"request: /beppe")
      reply(ui.supervisorStartPage("Hej handledare! Fyll i alla fält:")) 
    } } ~
    path("beppe" / "login") { get { 
      parameters("name", "course", "room", "state") { (n, c, r, s) =>
        log(s"request: /beppe/login?name=$n&course=$c&room=$r&state=$s")
        val u = db.addUser(n)
        log(s"added $u to userNamesToMap=${db.userNamesToMap}")
        val rOpt = db.addRoomIfEmpty(course = c, roomName = r, supervisorOpt = Some(u))
        log(s"room optionally added: $rOpt")
        val rOpt2 = db.addSupervisorToRoomIfNonEmpty(
          course = c, roomName = r, supervisor = u)
        log(s"supervisor $u added to room: $rOpt")
        reply(ui.supervisorUpdatePage(u.id, c, r, s))
    } } } ~
    path("beppe" / "update") { get { 
      parameters("userid", "course", "room", "state") { (u, c, r, s) =>
        log(s"request: /beppe/room?userid=$u&course=$c&room=$r&state=$s")
        s match {
          case "gone" => 
            log(s"hejdå handledare $u")
            val uOpt = db.User.fromString(u)
            val okOpt = uOpt.map(db.removeUser)
            if (!okOpt.getOrElse(false)) log(s"ERROR: removeUser $u $uOpt") 
            reply(ui.supervisorStartPage(s"Handledare $u har sagt hejdå."))
          
          case "super" => 
            log(s"super $u")
            reply(ui.supervisorUpdatePage(u, c, r, s)) 

          case _ => 
            log(s"ERROR: supervisor state unknown: $s")
            reply(ui.studentUpdatePage(u, c, r, s)) 
       }
    } } } ~
    path("sigrid") { get {
        log(s"request: /sigrid")
        reply(ui.studentStartPage("Hej student! Fyll i alla fält:")) 
    } } ~
    path("sigrid" / "login") { get { 
      parameters("name", "course", "room", "state") { (n, c, r, s) =>
        log(s"request: /sigrid/login?name=$n&course=$c&room=$r&state=$s")
        val u = db.addUser(n)
        log(s"added $u to userNamesToMap=${db.userNamesToMap}")
        val rOpt = db.addRoomIfEmpty(c, r, None)
        log(s"room optionally added: $rOpt")
        val rOpt2 = db.addStudentToRoomIfNonEmpty(u, c, r)
        reply(ui.studentUpdatePage(u.id, c, r, s))
    } } } ~
    path("sigrid" / "update") { get { 
      parameters("userid", "course", "room", "state") { (u, c, r, s) =>
        log(s"request: /sigrid/room?userid=$u&course=$c&room=$r&state=$s")
        s match {
          case "exit" => 
            log(s"hejdå student $u")
            val uOpt = db.User.fromString(u)
            val okOpt = uOpt.map(db.removeUser)
            if (!okOpt.getOrElse(false)) log(s"ERROR: removeUser $u $uOpt") 
            reply(ui.studentStartPage(s"Student $u har sagt hejdå."))
          
          case "help" => 
            val uOpt = db.User.fromString(u)
            val rOpt = uOpt.flatMap(u => db.wantHelp(u, c, r))
            log(s"help $u changed room to $rOpt")
            reply(ui.studentUpdatePage(u, c, r, s))

          case "ready" => 
            val uOpt = db.User.fromString(u)
            val rOpt = uOpt.flatMap(u => db.wantApproval(u, c, r))
            log(s"ready $u changed room to $rOpt")
            reply(ui.studentUpdatePage(u, c, r, s))

          case "work" => 
            val uOpt = db.User.fromString(u)
            val rOpt = uOpt.flatMap(u => db.working(u, c, r))
            log(s"work $u changed room to $rOpt")
            reply(ui.studentUpdatePage(u, c, r, s)) 

          case _ => 
            log(s"ERROR: student state unknown: $s")
            reply(ui.studentUpdatePage(u, c, r, s)) 
       }
    } } }
}
