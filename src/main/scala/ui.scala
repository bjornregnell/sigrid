object ui {
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
  |    <input name="name" id="name" value="" class="smallinput">
  |    Exempel: kim
  |    </br>
  |
  |    <label for="kurskod"><b>Kurskod:</b> </label>
  |    <input name="course" id="course" value="" class="smallinput">
  |    Exempel: EDAA45
  |    </br>
  |
  |    <label for="rum">  &nbsp;&nbsp;&nbsp;  <b>Rum:</b> </label>  
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
    ${html.h1(s"* $heading *")}
    <p> ${html.link("https://github.com/bjornregnell/sigrid/", "Sigrid")} är en hjälpköwebbapp @ ${Date.now.show} </p>
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
      .map(r => showRoomShort(r, course.isEmpty))
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
      |<form action="update" method="get">
      |  <div>
      |    <input type="hidden" name="userid" value="$userid">
      |    <input type="hidden" name="course" value="$course">
      |    <input type="hidden" name="room" value="$room">
      |<div class="radiobutton">
      |    <p><input type="radio" class="radiobutton" name="state" value="work"  ${check("work")}>  &nbsp;<b>Jobba på!</b> Stå inte i någon kö.</p>
      |    <p><input type="radio" name="state" value="help"  ${check("help")}> &nbsp;<b>Hjäälp!!!</b> Stå i hjälpkön.</p>
      |    <p><input type="radio" class="radiobutton" name="state" value="ready" ${check("ready")}> &nbsp;<b>Fäärdiig!</b> Stå i redovisningskön. </p>  
      |    <p><input type="radio" class="radiobutton" name="state" value="exit"  ${check("exit")}> &nbsp;<b>TackÅHej!</b> Logga ut från Sigrid. </p>  
      |</div>
      |   <p>Glöm ej <i>Jobba på! + Update</i> när du fått hjälp.</br>
      |      Glöm ej <i>TackÅHej! + Update</i> när du är klar.</p>
      |
      |    <button class="button">Update</button>
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
      |${html.h1(s"HANDLEDARE $userid i $room")}
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