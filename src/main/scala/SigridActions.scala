import akka.http.scaladsl.server.StandardRoute

/** The Sigrid web server actions generating a page reply for requests. 
 *  Mix with WebServer to create a SigridServer instance.
*/
trait SigridActions { 
  self: WebServer => // require this trait to be a WebServer mix-in

  def log(msg: String): Unit = println(s"\nSIGRID @ ${Date.now.show}> $msg")

  def supervisorLogin(name: String, course: String, room: String, state: String): StandardRoute = {
    log(s"request: /beppe/login?name=$name&course=$course&room=$room&state=$state")
    
    val u = db.addUser(name)
    log(s"added $u to userNamesToMap=${db.userNamesToMap}")
    
    val rOpt = db.addRoomIfEmpty(course, room, supervisor = Some(u))
    log(s"room added: $rOpt")
    
    val rOpt2 = 
      db.addSupervisorIfNonEmptyRoomAndSupervisorMissing(u,course,roomName = room)
    val sup: Option[User] = rOpt2.flatMap(_.supervisor)
    if (sup == Some(u)) {
      log(s"supervisor $u added to room: $rOpt")
      reply(ui.supervisorUpdatePage(u.id, course, room, state))
    } else {
      log(s"ERROR: room $rOpt already has supervisor, removing user $u")
      db.removeUser(u)
      reply(ui.supervisorStartPage(s"ERROR: Rummet har redan handledare: $sup"))
    }
  }

  def studentLogin(name: String, course: String, room: String, state: String): StandardRoute = {
    log(s"request: /sigrid/login?name=$name&course=$course&room=$room&state=$state")
    
    val u = db.addUser(name)
    log(s"added $u to userNamesToMap=${db.userNamesToMap}")
    
    val rOpt = db.addRoomIfEmpty(course, room, None)
    log(s"room optionally added: $rOpt")
    
    val rOpt2 = db.addStudentToRoomIfNonEmpty(u, course, room)
    reply(ui.studentUpdatePage(u.id, course, room, state))
  }

  def supervisorUpdate(u: String, c: String, r: String, s: String): StandardRoute = {
    log(s"request: /beppe/room?userid=$u&course=$c&room=$r&state=$s")
    s match {
      case "supervising" => 
        log(s"supervising $u")
        reply(ui.supervisorUpdatePage(u, c, r, s)) 

      case "clearhelp" => 
        log(s"clearhelp chosen by supervisor $u")
        val rOpt = db.clearHelpQueue(c,r)
        val err: String = if (rOpt.isEmpty) s"Error: $u" else u
        log(s"$err cleared help queue in $rOpt")
        reply(ui.supervisorUpdatePage(u, c, r, "supervising")) 

      case "clearready" => 
        log(s"clearready chosen by supervisor $u")
        val rOpt = db.clearApprovalQueue(c,r)
        val err: String = if (rOpt.isEmpty) s"Error: $u" else u
        log(s"$err cleared ready queue in $rOpt")
        reply(ui.supervisorUpdatePage(u, c, r, "supervising")) 

      case "gone" => 
        log(s"gone supervisor $u")
        val uOpt = User.fromUserId(u)
        val okOpt = uOpt.map(db.removeUser)
        if (!okOpt.getOrElse(false)) log(s"ERROR: removeUser $u $uOpt") 
        reply(ui.supervisorStartPage(s"Handledare $u har sagt hejdå."))

      case "purge" => 
        log(s"purge supervisor $u room $c $r ")
        val rOpt = db.removeRoom(c, r)
        val err: String = if (rOpt.isEmpty) s"Error: $u" else u
        log(s"$err removed room $c $rOpt")
        reply(ui.supervisorStartPage(s"Rummet raderades av $u")) 

      case _ => 
        log(s"ERROR: supervisor state unknown: $s")
        reply(ui.supervisorUpdatePage(u, c, r, "supervising")) 
   }
  }

  def studentUpdate(u: String, c: String, r: String, s: String): StandardRoute = {
    log(s"request: /sigrid/room?userid=$u&course=$c&room=$r&state=$s")
    s match {
      case "exit" => 
        log(s"hejdå student $u")
        val uOpt = User.fromUserId(u)
        val okOpt = uOpt.map(db.removeUser)
        if (!okOpt.getOrElse(false)) log(s"ERROR: removeUser $u $uOpt") 
        reply(ui.studentStartPage(s"Student $u har sagt hejdå."))
      
      case "help" => 
        val uOpt = User.fromUserId(u)
        val rOpt = uOpt.flatMap(u => db.wantHelp(u, c, r))
        log(s"help $u changed room to $rOpt")
        reply(ui.studentUpdatePage(u, c, r, s))

      case "ready" => 
        val uOpt = User.fromUserId(u)
        val rOpt = uOpt.flatMap(u => db.wantApproval(u, c, r))
        log(s"ready $u changed room to $rOpt")
        reply(ui.studentUpdatePage(u, c, r, s))

      case "work" => 
        val uOpt = User.fromUserId(u)
        val rOpt = uOpt.flatMap(u => db.working(u, c, r))
        log(s"work $u changed room to $rOpt")
        reply(ui.studentUpdatePage(u, c, r, s)) 

      case _ => 
        log(s"ERROR: student state unknown: $s")
        reply(ui.studentUpdatePage(u, c, r, "work")) 
    }
  }

}