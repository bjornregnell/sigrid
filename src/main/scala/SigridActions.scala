import akka.http.scaladsl.server.StandardRoute

/** The Sigrid web server actions generating a page reply for requests. 
 *  Mix with WebServer to create a SigridServer instance.
*/
trait SigridActions { 
  self: WebServer => // require this trait to be a WebServer mix-in

  def log(msg: String): Unit = println(s"\nSIGRID @ ${Date.now().show}> $msg")

  def supervisorLogin(name: String, course: String, room: String, state: String): StandardRoute = {
    log(s"request: /beppe/login?name=$name&course=$course&room=$room&state=$state")

    val nPurgedRooms = db.purgeRemovableRooms()
    if (nPurgedRooms > 0) log(s"purged $nPurgedRooms removable rooms")

    val nPurgedUsers = db.purgeRemovableUsers()
    if (nPurgedUsers > 0) log(s"purged $nPurgedUsers removable users")

    val u = db.addUser(name)
    log(s"added $u to userNamesToMap=${db.userNamesToMap}")
    
    val rOpt = db.addRoomIfNotExists(course, room)
    log(s"room before update: $rOpt")
    
    val rOpt2: Option[Room] = 
      db.addSupervisorIfRoomExists(u, course, roomName = room)

    val sup: Set[User] = rOpt2.map(_.supervisors).getOrElse(Set())
    if (sup.contains(u)) {
      log(s"supervisor $u added to room: $rOpt2")
      reply(ui.supervisorUpdatePage(u.id, course, room, state))
    } else {
      log(s"ERROR: could not add $u to $rOpt2")
      db.removeUser(u)
      reply(ui.supervisorStartPage(s"ERROR: kunde inte lägga till: $sup (förmodligen en bugg)"))
    }
  }

  def studentLogin(name: String, course: String, room: String, state: String): StandardRoute = {
    log(s"request: /sigrid/login?name=$name&course=$course&room=$room&state=$state")

    val nPurgedRooms = db.purgeRemovableRooms()
    if (nPurgedRooms > 0) log(s"purged $nPurgedRooms removable rooms")

    val nPurgedUsers = db.purgeRemovableUsers()
    if (nPurgedUsers > 0) log(s"purged $nPurgedUsers removable users")

    val u = db.addUser(name)
    log(s"added $u to userNamesToMap=${db.userNamesToMap}")
    
    val rOpt = db.addRoomIfNotExists(course, room)
    log(s"room before update: $rOpt")
    
    val rOpt2 = db.addStudentIfRoomExists(u, course, room)
    log(s"room after updated: $rOpt")

    reply(ui.studentUpdatePage(u.id, course, room, state))
  }

  def errorMessage(u: String, c: String, r: String, s: String, isValidState: Set[String]): Option[String] = {
    val uidOpt: Option[User] = User.fromUserId(u)
    val hasUser: Boolean = uidOpt.map(uid => db.hasUser(uid)).getOrElse(false)
    val result = 
      if (!db.hasRoom(c, r)) Some(s"ERROR: Rum $r i kurs $c saknas!")
      else if (!hasUser) Some(s"ERROR: $u saknas!")
      else if (!isValidState(s)) Some(s"ERROR: okänt tillstånd: $s")
      else None
    result.foreach(log)
    result
  }

  def supervisorUpdate(u: String, c: String, r: String, s: String, n: String): StandardRoute = {
    log(s"request: /beppe/room?userid=$u&course=$c&room=$r&state=$s&name=$n")
    errorMessage(u=u, c=c, r=r, s=s, isValidState=ui.validSupervisorState)
      .map(errMsg => reply(ui.supervisorStartPage(errMsg)))
      .getOrElse (
        s match {
          case "supervising" => 
            log(s"supervising $u")
            reply(ui.supervisorUpdatePage(u, c, r, s)) 

          case "pophelp" => 
            log(s"pophelp chosen by supervisor $u")
            val rOpt = db.popHelpQueue(c,r)
            val err: String = if (rOpt.isEmpty) s"Error: $u" else u
            log(s"$err pop help queue in $rOpt")
            reply(ui.supervisorUpdatePage(u, c, r, "supervising", "Hjälpkö poppad.")) 

          case "popready" => 
            log(s"popready chosen by supervisor $u")
            val rOpt = db.popApprovalQueue(c,r)
            val err: String = if (rOpt.isEmpty) s"Error: $u" else u
            log(s"$err pop ready queue in $rOpt")
            reply(ui.supervisorUpdatePage(u, c, r, "supervising", "Redovisningskö poppad.")) 

          case "clearhelp" => 
            log(s"clearhelp chosen by supervisor $u")
            val rOpt = db.clearHelpQueue(c,r)
            val err: String = if (rOpt.isEmpty) s"Error: $u" else u
            log(s"$err cleared help queue in $rOpt")
            reply(ui.supervisorUpdatePage(u, c, r, "supervising", "Hjälpkö tömd.")) 

          case "clearready" => 
            log(s"clearready chosen by supervisor $u")
            val rOpt = db.clearApprovalQueue(c,r)
            val err: String = if (rOpt.isEmpty) s"Error: $u" else u
            log(s"$err cleared ready queue in $rOpt")
            reply(ui.supervisorUpdatePage(u, c, r, "supervising", "Redovisningskö tömd.")) 

          case "removeuser" =>
            log(s"state removeuser: supervisor=$u; try remove name=$n")
            if (u != n) {
              val uOpt = User.fromUserId(n)
              val okOpt = uOpt.map(db.removeUser)
              if (!okOpt.getOrElse(false)) {
                log(s"ERROR: cannot remove $n not in ${db.users}")
                reply(ui.supervisorUpdatePage(u, c, r, "supervising", s"ERROR: användare $n saknas i ${db.users}"))
              } else {
                log(s"removing user $n requested by $u in room $r in course $c")
                reply(ui.supervisorUpdatePage(u, c, r, "supervising", s"Användare $n borttagen"))
              }
            } else {
              log(s"ERROR: attempt to remove self $u")
              reply(ui.supervisorUpdatePage(u, c, r, "supervising", s"ERROR: du kan inte ta bort dig själv $n"))
            }
            

          case "gone" => 
            log(s"gone supervisor $u")
            val uOpt = User.fromUserId(u)
            val okOpt = uOpt.map{ u => 
              val _ = db.goodbye(u, c, r)
              db.removeUserIfNotInAnyRoom(u)
            }
            if (!okOpt.getOrElse(false)) log(s"ERROR: removeUser $u $uOpt") 
            reply(ui.supervisorStartPage(s"Handledare $u har sagt hejdå."))

          case "purge" => 
            log(s"purge supervisor $u room $c $r ")
            val rOpt = db.removeRoom(c, r)
            val err: String = if (rOpt.isEmpty) s"Error: $u" else u

            val uOpt = User.fromUserId(u)
            val okOpt = uOpt.map(u => db.removeUserIfNotInAnyRoom(u))
            if (!okOpt.getOrElse(false)) log(s"ERROR: removeUser $u $uOpt") 

            log(s"$err removed room $c $rOpt")
            reply(ui.supervisorStartPage(s"Rummet raderades av $u")) 

          case _ => 
            log(s"ERROR: supervisor state unknown: $s for user=$u course=$c room=$r")
            reply(ui.supervisorUpdatePage(u, c, r, "supervising", "")) 
        }
      )
  }

  def studentUpdate(u: String, c: String, r: String, s: String): StandardRoute = {
    log(s"request: /sigrid/room?userid=$u&course=$c&room=$r&state=$s")
    errorMessage(u=u, c=c, r=r, s=s, isValidState=ui.validStudentState)
      .map(errMsg => reply(ui.studentStartPage(errMsg)))
      .getOrElse (
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
      )
  }

}