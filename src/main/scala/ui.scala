object ui {
  def sigrid: String = "Sigrid"

  def sigridLink: String = "https://www.youtube.com/watch?v=cc-TAuKWdTI"

  def sigridEmbeddedLink: String = """
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
    
    $sigrid är ${html.link(sigridLink, "fäärdiig!!!")} </br>

    $sigridEmbeddedLink

    """
  )

  def studentLoginForm: String = s"""
    |<form action="/sigrid/login" method="get">
    |  <div>
    |    <label for="name"><b>Ditt förnamn:</b> </label>
    |    <input name="name" id="name" value="" class="mediuminput">
    |    Exempel: kim
    |    </br>
    |
    |    <label for="kurskod"><b>Kurskod:</b> </label>
    |    <input name="course" id="course" value="" class="smallinput">
    |    Exempel: edaa45
    |    </br>
    |
    |    <label for="rum"><b>Rum:</b> </label>
    |    <input name="room" id="room" value="" class="smallinput">
    |    Exempel: hacke
    |    </br>
    |
    |    <input type="hidden" name="state" value="work">
    |
    |    <button class="button">Enter</button>
    |  </div>
    |</form>
    |""".stripMargin

  def studentUpdatePage(userid: String, course: String, room: String, state: String): String = {
    def check(value: String) = if (value == state) """checked="checked" """ else ""
    html.page(title = s"SIGRID: $userid $state", body = s"""
      |${html.h1(s"=== $userid är i rum $room ===")}
      |<p> SIGRID KÖAR. Sidan uppdaterad: ${new java.util.Date} </p>
      |<form action="update" method="get">
      |  <div>
      |    <label for="userid">Användare: <b>$userid</b> </label>
      |    <label for="kurskod">Kurs: <b>$course</b> </label>
      |    <label for="rum">Rum:<b>$room</b> </label>
      |    </br>
      |
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
      |$showData
      |""".stripMargin
    )
  }

  def sigridHeader: String = s"""
      ${html.h1("=== SIGRID  ===")}
      <p> Sigrid är en hjälpköwebbapp @ ${new java.util.Date} </p>
      <p> Kolla koden: 
      ${html.link(
        url="https://github.com/bjornregnell/sigrid/", 
        text="github.com/bjornregnell/sigrid")} </p>
  """

  def showData: String = s"""
    Users: ${db.users} </br>
    Rooms:  ${db.roomsToMap}
  """

  def startPage: String = html.page(title = "SIGRID LOGIN", body =
    s"""
      $sigridHeader
      $studentLoginForm
      $showData
    """
  )

}