object ui {
  val MonitorReloadEverySeconds = 10

  val validStudentState = Set("work", "help", "ready", "exit")
  
  val validSupervisorState = Set("supervising", "pophelp", "popready", "clearhelp", "clearready", "removeuser", "mergeroom", "gone", "purge")
 
  def showRawDatabase: String = s"""
    <p><br>----- raw database toStrings for debugging purposes -----<br> 
      users: ${db.users.size} ${db.users} <br>
      <br>
      rooms: ${db.roomsToMap.size} ${db.roomsToMap} <br>
    </p>
    """

  def loginForm(msg: String = "", action: String, state: String): String = s"""
  |<form action="$action" method="get">
  |  <div>
  |    <p> $msg </p>
  |    <label for="name">   <b>Förnamn:</b> </label>
  |    <input name="name" id="name" value="" class="smallinput" required>
  |    Exempel: <b>kim</b>
  |    <br>
  |
  |    <label for="kurskod"> &nbsp;&nbsp;&nbsp;<b>Kurs:</b> </label>
  |    <input name="course" id="course" value="${RoomKey.DefaultCourse}" class="smallinput" required>
  |    Exempel: <b>${RoomKey.DefaultCourse}</b>, dod, ...
  |    <br>
  |
  |    <label for="rum">  &nbsp;&nbsp;&nbsp;  <b>Rum:</b> </label>  
  |    <input name="room" id="room" value="" class="smallinput" required>
  |    Exempel: <b>Hacke</b> (<b>Distans</b> om fjärran) 
  |    <br>
  |
  |    <input type="hidden" name="state" value="$state">
  |
  |    <button class="button">Logga in</button> 
  |    ${html.link("http://cs.lth.se/sigrid/monitor", "SIGRID MONITOR")} 
  |  </div>
  |</form>
  |""".stripMargin

  def sigridHeader(heading: String): String = s"""
    ${html.h1(s"* $heading *")}
    <p> ${html.link("http://cs.lth.se/sigrid", "Sigrid")} är en hjälpköwebbapp @ ${Date.now().show} </p>
    <p> Karta över ${html.link("https://fileadmin.cs.lth.se/cs/Bilder/Salar/Datorsalar_E-huset.pdf", "E-husets datorrum")}. Kolla ${html.link("https://github.com/bjornregnell/sigrid/", "koden")}.</p>
  """

  def studentStartPage(msg: String = "Hej student!"): String = 
    html.page(
      title = "SIGRID LOGIN", 
      body =s"""
        |  ${sigridHeader("SIGRID")}
        |  ${loginForm(msg, action = "/sigrid/login", state = "work")}
        |  ${showAllRooms(course = None, exceptRoom = None)}
        |  $showRawDatabase
        |""".stripMargin
    )

  def supervisorStartPage(msg: String = "Hej handledare!"): String = 
    html.page(
      title = "BEPPE LOGIN", 
      body = s"""
       |   ${sigridHeader("BEPPE")}
       |   ${loginForm(msg, action = "/beppe/login", state = "supervising")}
       |   ${showAllRooms(course = None, exceptRoom = None)}
       |   $showRawDatabase
       |""".stripMargin
    ) 
  
  def monitorPage(): String = 
    html.page(
      title = "SIGRID MONITOR", 
      body = s"""
      |   ${sigridHeader("SIGRID MONITOR")}
      |   ${if (db.rooms.isEmpty) "INGA AKTIVA RUM" else ""}
      |   ${showAllRooms(course = None, exceptRoom = None, isShortVersion = false)}
      |
      |<p>Denna sida uppdateras med $MonitorReloadEverySeconds sekunders intervall.</p>
      |$showRawDatabase
      |
      |""".stripMargin,
      reloadEverySeconds = MonitorReloadEverySeconds
    ) 

  def showQueueLength(qname: String, n: Int): String = s"""
    ${html.boldIf(n > 0)(qname)}: $n
  """
  
  def showRoomShort(r: Room, isShowCourse: Boolean = false): String = s"""
    &nbsp; &nbsp;
    ${if (isShowCourse) r.course else ""} ${r.name}: ${r.students.size} studenter, 
    handl ${r.supervisors.map(_.id)}, 
    ${showQueueLength("hjälp",r.helpQueue.length)},
    ${showQueueLength("redov", r.approvalQueue.length)}
    ${if (r.maxQueuingTime() > 0) f", Köat: ${r.maxQueuingTime()} min" else ""}
  
  """
  
  def showRoomLong(roomName: String, course: String): String = {
    val r = db.roomsToMap(RoomKey(course, roomName))
    val n = r.students.size
    s"""
    <p>  $course  &nbsp; 
      <b>${r.name}:</b> $n student${if (n != 1) "er" else ""}, 
      handledare ${r.supervisors.map(_.id)} <br>

      &nbsp;&nbsp; <i>hjälpkö:</i> 
      ${r.helpQueue.length} 
      ${r.helpQueueString()} <br>

      &nbsp;&nbsp; <i>redovkö:</i> 
      ${r.approvalQueue.length} 
      ${r.approvalQueueString()}
    </p>
    """
  }

  def showAllRooms(exceptRoom: Option[String] = None, course: Option[String] = None, isShortVersion: Boolean = true): String = {
    val delim = "<br>\n"
    def roomFilter(r: Room): Boolean = exceptRoom.map(_ != r.name).getOrElse(true) 
    def courseFilter(r: Room): Boolean = course.map(_ == r.course).getOrElse(true) 
    val table = db.rooms
      .filter(r => roomFilter(r) && courseFilter(r))
      .sortBy(r => r.course + r.name)
      .map(r => if (isShortVersion) showRoomShort(r, course.isEmpty) else showRoomLong(r.name, r.course))
      .mkString(delim)
    val heading = 
      if (table.nonEmpty) 
        s"""&nbsp;${if (exceptRoom.nonEmpty) "Övriga" else "Aktiva"} rum:<br>"""
      else ""
    s"$heading \n $table"
  }

  def studentUpdatePage(userid: String, course: String, room: String, state: String, msg: String = ""): String = {
    def check(value: String) = 
      if (value == state) """checked="checked" """ else ""

    html.page(title = s"SIGRID: $userid $state", body = s"""
      |${html.h1(s"STUDENT $userid ${if (room == "Distans") "på" else "i"} $room")}
      |${RoomKey.roomWarning(room)}
      |${if (msg.nonEmpty) s"<p> $msg </p>" else ""}
      |<p>Välj tillstånd och klicka på gröna <i>Uppdatera</i>-knappen.</p> 
      |<form action="update" method="get">
      |<div>
      |  <input type="hidden" name="userid" value="$userid">
      |  <input type="hidden" name="course" value="$course">
      |  <input type="hidden" name="room" value="$room">
      |
      |  <div class="radio-toolbar">
      |
      |  <p><input type="radio" id="radioWork" name="state" value="work"  ${check("work")}>  
      |  <label for="radioWork">Köar inte</label>&nbsp; Jobbar eller får hjälp.</p>
      |
      |  <p><input type="radio" id="radioHelp" name="state" value="help"  ${check("help")}> 
      |  <label for="radioHelp">Hjäälp!!!</label>&nbsp; Står i hjälpkön.</p>
      |
      |  <p><input type="radio" id="radioReady" name="state" value="ready" ${check("ready")}> 
      |  <label for="radioReady">Fäärdiig!</label>&nbsp; Står i redovisningskön. </p> 
      | 
      |  <p><input type="radio" id="radioExit" name="state" value="exit"  ${check("exit")}> 
      |  <label for="radioExit">Loggar ut</label>&nbsp; Redovisar, lämnar rummet. </p> 
      | 
      |</div>
      |   <p>Glöm inte <i>Köar inte + Uppdatera</i> medan du får hjälp.<br>
      |      Glöm inte <i>Loggar ut + Uppdatera</i> medan du redovisar.</p>
      |
      |    <button class="button">Uppdatera</button>
      |</div>
      |</form>
      |<p> ${course} ${Date.now().show} </p>
      |${showRoomLong(roomName = room, course = course)}
      |${showAllRooms(course = Some(course), exceptRoom = Some(room))}
      |$showRawDatabase
      |""".stripMargin
    )
  }
//           &nbsp;<b>Jobba på!</b> Stå inte i någon kö.</p>
//

  def supervisorUpdatePage(userid: String, course: String, room: String, state: String, msg: String = ""): String = {
    def check(value: String) = if (value == state) """checked="checked" """ else ""
  
    html.page(title = s"BEPPE: $userid $state", body = s"""
      |${html.h1(s"HANDLEDARE $userid i $room")}
      |${RoomKey.roomWarning(room)}
      |${if (msg.nonEmpty) s"<p>$msg</p>" else ""}
      |<form action="update" method="get">
      |  <div class="radio-toolbar">
      |
      |  <input type="hidden" name="userid" value="$userid">
      |  <input type="hidden" name="course" value="$course">
      |  <input type="hidden" name="room" value="$room">
      |
      |  <p><input type="radio" id="radioSup" name="state" value="supervising" ${check("supervising")}>
      |  <label for="radioSup"><b>Jubba!</b></label> &nbsp; Handledare handleder.</p>
      |
      |  <p><input type="radio" id="radioPopHelp" name="state" value="pophelp" ${check("pophelp")}>
      |  <label for="radioPopHelp"><b>Pop hjälpkö</b></label> &nbsp; Ta bort första student ur hjälpkön.</p>
      |
      |  <p><input type="radio" id="radioPopReady" name="state" value="popready" ${check("popready")}>
      |  <label for="radioPopReady"><b>Pop redovkö</b></label> &nbsp; Ta bort första student ur redovkön.</p>
      | 
      |  <p><input type="radio" id="radioClearHelp" name="state" value="clearhelp" ${check("clearhelp")}>
      |  <label for="radioClearHelp"><b>Töm hjälpkö</b></label> &nbsp; Töm hjälpkön. VARNING!</p>
      |
      |  <p><input type="radio" id="radioClearReady" name="state" value="clearready" ${check("clearready")}>
      |  <label for="radioClearReady"><b>Töm redovkö</b></label> &nbsp; Töm redovisningskön. VARNING!</p>
      | 
      |  <p><input type="radio" id="radioRemoveStudent" name="state" value="removeuser" ${check("removeuser")}>
      |  <label for="radioRemoveStudent"><b>Tabort användare</b></label> &nbsp; 
      |  namn-nr: <input name="name" id="name" value="" class="smallinput"> VARNING!</p>
      | 
      |  <p><input type="radio" id="radioMergeRoom" name="state" value="mergeroom" ${check("mergeroom")}>
      |  <label for="radioMergeRoom"><b>Slå ihop rum</b></label> &nbsp; 
      |  Andra rummet: <input name="other" id="other" value="" class="smallinput"> VARNING!</p>
      | 
      |  <p><input type="radio" id="radioGone" name="state" value="gone"  ${check("gone")}> 
      |  <label for="radioGone"><b>Hejdå</b></label> &nbsp; Handledare lämnar,  rummet finns kvar.</p> 
      |  
      |  <p><input type="radio" id="radioPurge" name="state" value="purge"  ${check("purge")}> 
      |  <label for="radioPurge"><b>Radera</b></label> &nbsp; Radera rummet och dess köer. VARNING! </p> 
      |
      |  <p>Glöm ej <i>Radera + Uppdatera</i> när undervisningen är klar.</p>
      |
      |  <button class="button">Uppdatera</button>
      |  </div>
      |</form>
      |<p> ${course} ${Date.now().show} </p>
      |${showRoomLong(roomName = room, course = course)}
      |${showAllRooms(course = Some(course), exceptRoom = Some(room))}
      |$showRawDatabase
      |""".stripMargin
    )
  }

  def sigridVideoLink: String = "https://www.youtube.com/watch?v=cc-TAuKWdTI"

  def sigridVideoEmbeddedLink: String = """
    |<iframe width="560" height="315" 
    |  src="https://www.youtube.com/embed/cc-TAuKWdTI" 
    |  frameborder="0" 
    |  allow=
    |    "accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" 
    |  allowfullscreen>
    |</iframe>
    |""".trim.stripMargin

  def helloPage: String = html.helloPage(
    s"""
    
    Sigrid är ${html.link(sigridVideoLink, "fäärdiig!!!")} <br>

    $sigridVideoEmbeddedLink

    """
  )

}
