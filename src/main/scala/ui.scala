object ui {

  val validStudentState = Set("work", "help", "ready", "exit")
  
  val validSupervisorState = Set("supervising", "clearhelp", "clearready", "gone", "purge")
 
  def showRawDatabase: String = s"""
    <p></br>----- raw database toStrings for debugging purposes -----</br> 
      users: ${db.users.size} ${db.users} </br>
      </br>
      rooms: ${db.roomsToMap.size} ${db.roomsToMap} </br>
    </p>
    """

  def loginForm(msg: String = "", action: String, state: String): String = s"""
  |<form action="$action" method="get">
  |  <div>
  |    <p> $msg </p>
  |    <label for="name">   <b>Förnamn:</b> </label>
  |    <input name="name" id="name" value="" class="smallinput" required>
  |    Exempel: kim
  |    </br>
  |
  |    <label for="kurskod"><b>Kurskod:</b> </label>
  |    <input name="course" id="course" value="" class="smallinput" required>
  |    Exempel: EDAA45
  |    </br>
  |
  |    <label for="rum">  &nbsp;&nbsp;&nbsp;  <b>Rum:</b> </label>  
  |    <input name="room" id="room" value="" class="smallinput"  required>
  |    Exempel: Hacke 
  |    </br>
  |
  |    <input type="hidden" name="state" value="$state">
  |
  |    <button class="button">Enter</button> 
  |    ${html.link("sigrid/monitor", "SIGRID MONITOR")} 
  |  </div>
  |</form>
  |""".stripMargin

  def sigridHeader(heading: String): String = s"""
    ${html.h1(s"* $heading *")}
    <p> ${html.link("https://github.com/bjornregnell/sigrid/", "Sigrid")} är en hjälpköwebbapp @ ${Date.now.show} </p>
    <p> Karta över ${html.link("https://fileadmin.cs.lth.se/cs/Bilder/Salar/Datorsalar_E-huset.pdf", "E-husets datorrum")}. </p>
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
      |   ${showAllRooms(course = None, exceptRoom = None, isShortVersion = false)}
      |""".stripMargin
    ) 

  def showQueueLength(qname: String, n: Int): String = s"""
    ${html.boldIf(n > 0)(qname)}: $n
  """
  
  def showRoomShort(r: Room, isShowCourse: Boolean = false): String = s"""
    &nbsp; &nbsp;
    ${if (isShowCourse) r.course else ""} ${r.name}: ${r.students.size} studenter, 
    handl ${r.supervisor.map(_.id).getOrElse("<b>SAKNAS</b>")}, 
    ${showQueueLength("hjälp",r.helpQueue.length)},
    ${showQueueLength("redov", r.approvalQueue.length)}  
  """
  
  def showRoomLong(roomName: String, course: String): String = {
    val r = db.roomsToMap(RoomKey(course, roomName))
    val n = r.students.size
    s"""
    <p>  $course  &nbsp; 
      <b>${r.name}:</b> $n student${if (n != 1) "er" else ""}, 
      handledare ${r.supervisor.map(_.id).getOrElse("<b>SAKNAS</b>")} </br>

      &nbsp;&nbsp; <i>hjälpkö:</i> 
      ${r.helpQueue.length} 
      ${r.helpQueue.mkString(",")} </br>

      &nbsp;&nbsp; <i>redovkö:</i> 
      ${r.approvalQueue.length} 
      ${r.approvalQueue.mkString(",")}
    </p>
    """
  }

  def showAllRooms(exceptRoom: Option[String] = None, course: Option[String] = None, isShortVersion: Boolean = false): String = {
    val delim = "\n"
    def roomFilter(r: Room): Boolean = exceptRoom.map(_ != r.name).getOrElse(true) 
    def courseFilter(r: Room): Boolean = course.map(_ == r.course).getOrElse(true) 
    val table = db.rooms
      .filter(r => roomFilter(r) && courseFilter(r))
      .sortBy(r => r.course + r.name)
      .map(r => if (isShortVersion) showRoomShort(r, course.isEmpty) else showRoomLong(r.name, r.course))
      .mkString(delim)
    val heading = 
      if (table.nonEmpty) 
        s"""&nbsp;${if (exceptRoom.nonEmpty) "Övriga" else "Aktiva"} rum:</br>"""
      else ""
    s"$heading \n $table"
  }

  def studentUpdatePage(userid: String, course: String, room: String, state: String): String = {
    def check(value: String) = 
      if (value == state) """checked="checked" """ else ""

    html.page(title = s"SIGRID: $userid $state", body = s"""
      |${html.h1(s"STUDENT $userid i $room")}
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
      |  <label for="radioWork"><b>Jobba på!</b></label>&nbsp; Stå inte i någon kö.</p>
      |
      |  <p><input type="radio" id="radioHelp" name="state" value="help"  ${check("help")}> 
      |  <label for="radioHelp"><b>Hjäälp!!!</b></label>&nbsp; Ställ mig i hjälpkön.</p>
      |
      |  <p><input type="radio" id="radioReady" name="state" value="ready" ${check("ready")}> 
      |  <label for="radioReady"><b>Fäärdiig!</b></label>&nbsp; Ställ mig i redovisningskön. </p> 
      | 
      |  <p><input type="radio" id="radioExit" name="state" value="exit"  ${check("exit")}> 
      |  <label for="radioExit"><b>TackÅHej!</b></label>&nbsp; Lämna rummet. </p> 
      | 
      |</div>
      |   <p>Glöm ej <i>Jobba på! + Uppdatera</i> när du fått hjälp.</br>
      |      Glöm ej <i>TackÅHej! + Uppdatera</i> när du är klar.</p>
      |
      |    <button class="button">Uppdatera</button>
      |</div>
      |</form>
      |<p> ${course} ${Date.now.show} </p>
      |${showRoomLong(roomName = room, course = course)}
      |${showAllRooms(course = Some(course), exceptRoom = Some(room))}
      |$showRawDatabase
      |""".stripMargin
    )
  }
//           &nbsp;<b>Jobba på!</b> Stå inte i någon kö.</p>
//

  def supervisorUpdatePage(userid: String, course: String, room: String, state: String): String = {
    def check(value: String) = if (value == state) """checked="checked" """ else ""
  
    html.page(title = s"BEPPE: $userid $state", body = s"""
      |${html.h1(s"HANDLEDARE $userid i $room")}
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
      |  <p><input type="radio" id="radioClearHelp" name="state" value="clearhelp" ${check("clearhelp")}>
      |  <label for="radioClearHelp"><b>Töm hjälpkö!</b></label> &nbsp; Töm hjälpkön.</p>
      |
      |  <p><input type="radio" id="radioClearReady" name="state" value="clearready" ${check("clearready")}>
      |  <label for="radioClearReady"><b>Töm redovkö!</b></label> &nbsp; Töm redovisningskön.</p>
      | 
      |  <p><input type="radio" id="radioGone" name="state" value="gone"  ${check("gone")}> 
      |  <label for="radioGone"><b>Hejdå!</b></label> &nbsp; Handledare lämnar,  rummet finns kvar.</p> 
      |  
      |  <p><input type="radio" id="radioPurge" name="state" value="purge"  ${check("purge")}> 
      |  <label for="radioPurge"><b>Radera!</b></label> &nbsp; Radera rummet och dess köer.</p> 
      |
      |   <p>Glöm ej <i>Radera! + Uppdatera</i> när undervisningen är klar.</p>
      |
      |   <button class="button">Uppdatera</button>
      |  </div>
      |</form>
      |<p> ${course} ${Date.now.show} </p>
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
    
    Sigrid är ${html.link(sigridVideoLink, "fäärdiig!!!")} </br>

    $sigridVideoEmbeddedLink

    """
  )

}