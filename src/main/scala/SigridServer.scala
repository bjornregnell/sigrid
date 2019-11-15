import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.StandardRoute

object SigridServer extends WebServer with SigridActions {
  private def low(u: String): String = u.toLowerCase 
  private def cap(n: String): String = n.toLowerCase.capitalize
  private def up (c: String): String = c.toUpperCase

  override def routes =
    path("hello") { get { log(s"request: /hello"); reply(ui.helloPage) } }  ~
    path("beppe") { get {
      log(s"request: /beppe")
      reply(ui.supervisorStartPage("Hej handledare! Fyll i alla fält:")) 
    } } ~
    path("beppe" / "login") { get { 
      parameters("name", "course", "room", "state") { (n, c, r, s) =>
        supervisorLogin(low(n), up(c), cap(r), low(s))
    } } } ~
    path("beppe" / "update") { get { 
      parameters("userid", "course", "room", "state") { (u, c, r, s) =>
        supervisorUpdate(low(u), up(c), cap(r), low(s))
    } } } ~
    path("sigrid") { get {
        log(s"request: /sigrid")
        reply(ui.studentStartPage("Hej student! Fyll i alla fält:")) 
    } } ~
    path("sigrid" / "login") { get { 
      parameters("name", "course", "room", "state") { (n, c, r, s) =>
        studentLogin(low(n), up(c), cap(r), low(s))
    } } } ~
    path("sigrid" / "update") { get { 
      parameters("userid", "course", "room", "state") { (u, c, r, s) =>
        studentUpdate(low(u), up(c), cap(r), low(s))
    } } }

}