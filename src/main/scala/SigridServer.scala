import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.StandardRoute

/** Sigrid web server url routing, delegating to SigridActions. */
object SigridServer extends WebServer with SigridActions {

  // helper methods to make input string formats valid up-front:
  private def vn(name: String): String   = User.validName(name) 
  private def vc(course: String): String = RoomKey.validCourse(course)
  private def vr(room: String): String   = RoomKey.validRoomName(room)
  private def vs(state: String): String  = state.filter(_.isLetter).toLowerCase
  private def vu(uid: String): String    = User.validUserId(uid)

  /* Routing of incoming web server requests, delegating to SigridActions. */
  override def routes =
    path("hello") { get { log(s"request: /hello"); reply(ui.helloPage) } }  ~
    path("beppe") { get {
      log(s"request: /beppe")
      db.purgeRemovableRooms()
      reply(ui.supervisorStartPage()) 
    } } ~
    path("beppe" / "login") { get { 
      parameters("name", "course", "room", "state") { (n, c, r, s) =>
        supervisorLogin(vn(n), vc(c), vr(r), vs(s))
    } } } ~
    path("beppe" / "update") { get { 
      parameters("userid", "course", "room", "state", "name", "other") { (u, c, r, s, n, o) =>
        println(s"*** DEBUG: parameters($u, $c, $r, $s, $n, $o)")
        supervisorUpdate(vu(u), vc(c), vr(r), vs(s), vu(n), vr(o))
    } } } ~
    path("sigrid") { get {
        log(s"request: /sigrid")
        db.purgeRemovableRooms()
        reply(ui.studentStartPage()) 
    } } ~
    path("sigrid" / "monitor") { get {
      print(".")
      db.purgeRemovableRooms()
      reply(ui.monitorPage()) 
    } } ~
    path("sigrid" / "login") { get { 
      parameters("name", "course", "room", "state") { (n, c, r, s) =>
        studentLogin(vn(n), vc(c), vr(r), vs(s))
    } } } ~
    path("sigrid" / "update") { get { 
      parameters("userid", "course", "room", "state") { (u, c, r, s) =>
        studentUpdate(vu(u), vc(c), vr(r), vs(s))
    } } }

}