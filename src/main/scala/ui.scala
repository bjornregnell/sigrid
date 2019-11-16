object ui {
  def showRawDatabase: String = s"""
    <p></br>--- raw database toStrings for debuging ---</br> 
      <b>users:</b> ${db.users} </br>
      </br>
      <b>rooms:</b> ${db.roomsToMap} </br>
    </p>
    """

  def loginForm(msg: String = "", action: String, state: String): String = s"""
  |<form action="$action" method="get">
  |  <div>
  |    <p> $msg </p>
  |    <label for="name"><b>Ditt förnamn:</b> </label>
  |    <input name="name" id="name" value="" class="mediuminput">
  |    Exempel: kim
  |    </br>
  |
  |    <label for="kurskod"><b>Kurskod:</b> </label>
  |    <input name="course" id="course" value="" class="smallinput">
  |    Exempel: EDAA45
  |    </br>
  |
  |    <label for="rum"><b>Rum:</b> </label>
  |    <input name="room" id="room" value="" class="smallinput">
  |    Exempel: Hacke 
  |    </br>
  |
  |    <input type="hidden" name="state" value="$state">
  |
  |    <button class="button">Enter</button>
  |  </div>
  |</form>
  |""".stripMargin

  def sigridHeader(heading: String): String = s"""
    ${html.h1(s"=== $heading  ===")}
    <p> ${html.link("/hello", "Sigrid")} är en hjälpköwebbapp @ ${new java.util.Date} </p>
    <p> Kolla koden: 
    ${html.link(
      url="https://github.com/bjornregnell/sigrid/", 
      text="github.com/bjornregnell/sigrid")} </p>
  """

  def studentStartPage(msg: String = "Hej student! Fyll i alla fält:"): String = 
    html.page(
      title = "SIGRID LOGIN", 
      body =
        s"""
          ${sigridHeader("SIGRID")}
          ${loginForm(msg, action = "/sigrid/login", state = "work")}
          $showRawDatabase
        """
    )

  def supervisorStartPage(msg: String = "Hej handledare! Fyll i alla fält:"): String = 
    html.page(
      title = "BEPPE LOGIN", 
      body =
        s"""
          ${sigridHeader("BEPPE")}
          ${loginForm(msg, action = "/beppe/login", state = "supervising")}
          $showRawDatabase
        """
    )

  def showQueueLength(qname: String, n: Int): String = s"""
    ${html.boldIf(n > 0)(qname)}: $n
  """
  
  def showRoomShort(r: Room): String = s"""
    &nbsp; &nbsp;
    ${r.name}: ${r.students.size} studenter, 
    handl ${r.supervisor.map(_.id).getOrElse("<b>SAKNAS</b>")}, 
    ${showQueueLength("hjälp",r.helpQueue.length)},
    ${showQueueLength("redov", r.approvalQueue.length)}  
  """
  
  def showRoomLong(roomName: String, course: String): String = {
    val r = db.roomsToMap(RoomKey(course, roomName))
    val n = r.students.size
    s"""
    <p>    &nbsp; 
      <b>${r.name}:</b> $n student${if (n != 1) "er" else ""}, 
      handledare ${r.supervisor.map(_.id).getOrElse("<b>SAKNAS</b>")} </br>

      &nbsp;&nbsp; <i>hjälpkö:</i> 
      ${r.helpQueue.length} 
      ${r.helpQueue.mkString(",")} </br>

      &nbsp;&nbsp; <i>redovkö:</i> 
      ${r.approvalQueue.length} 
      ${r.approvalQueue.mkString(",")} </br>
    </p>
    """
  }

  def showAllRooms(exceptRoom: Option[String] = None, course: Option[String] = None): String = {
    val delim = "</br>\n"
    def roomFilter(r: Room): Boolean = exceptRoom.map(_ != r.name).getOrElse(true) 
    def courseFilter(r: Room): Boolean = course.map(_ == r.course).getOrElse(true) 
    val table = db.rooms
      .filter(r => roomFilter(r) && courseFilter(r))
      .map(showRoomShort)
      .mkString(delim)
    val heading = 
      if (table.nonEmpty) 
        s"""&nbsp;${if (exceptRoom.nonEmpty) "Övriga" else "Alla"} rum:</br>"""
      else ""
    s"$heading \n $table"
  }

  def studentUpdatePage(userid: String, course: String, room: String, state: String): String = {
    def check(value: String) = 
      if (value == state) """checked="checked" """ else ""

    html.page(title = s"SIGRID: $userid $state", body = s"""
      |${html.h1(s"=== STUDENT $userid i $room ===")}
      |<form action="update" method="get">
      |  <div>
      |    <input type="hidden" name="userid" value="$userid">
      |    <input type="hidden" name="course" value="$course">
      |    <input type="hidden" name="room" value="$room">
      |
      |    <input type="radio" name="state" value="work"  ${check("work")}> Ej i kö. Jobbar!<br>
      |    <input type="radio" name="state" value="help"  ${check("help")}> Hjälpkö. Vill ha hjälp!<br>
      |    <input type="radio" name="state" value="ready" ${check("ready")}> Redovisningskö. Fääärdiiig! <br>  
      |    <input type="radio" name="state" value="exit"  ${check("exit")}> Lämna rummet. Hejdå!<br>  
      |    </br>
      |    <button class="button">Uppdatera</button>
      |  </div>
      |</form>
      |<p> ${course} ${Date.now.show} </p>
      |${showRoomLong(roomName = room, course = course)}
      |${showAllRooms(course = Some(course), exceptRoom = Some(room))}
      |$showRawDatabase
      |""".stripMargin
    )
  }

  def supervisorUpdatePage(userid: String, course: String, room: String, state: String): String = {
    def check(value: String) = if (value == state) """checked="checked" """ else ""
  
    html.page(title = s"BEPPE: $userid $state", body = s"""
      |${html.h1(s"=== HANDLEDARE $userid i $room ===")}
      |<form action="update" method="get">
      |  <div>
      |
      |  <input type="hidden" name="userid" value="$userid">
      |  <input type="hidden" name="course" value="$course">
      |  <input type="hidden" name="room" value="$room">
      |
      |  <input type="radio" name="state" value="supervising" ${check("supervising")}>
      |        <b>Jubba!</b> Handledare handleder!<br>
      | 
      |  <input type="radio" name="state" value="gone"  ${check("gone")}> 
      |        <b>Hejdå!</b> Handledare försvinner!<br> 
      |  
      |   <p>Glöm inte <i>Hejdå! + Uppdatera</i> när undervisningen är klar.</p>
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