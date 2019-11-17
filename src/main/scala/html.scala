object html {
    val styles = """
    |html { -webkit-font-smoothing: antialiased; }
    |body {
    |    background-color: pink;
    |    font-size: 28px;
    |    line-height: 1.2;
    |    font-family: "Lucida Console", Monaco, monospace;
    |    margin: 20px 20px;
    |}
    |.button {
    |    background-color: #4CAF50;
    |    border: none;
    |    color: pink;
    |    padding: 20px 80px;
    |    text-align: center;
    |    text-decoration: none;
    |    display: inline-block;
    |    font-size: 150%;
    |    font-weight:bold;
    |    margin: 20px 0px;
    |    cursor: pointer;
    |    font-family: "Lucida Console", Monaco, monospace;
    |}
    |.smallinput {
    |    width: 180px;
    |    font-size: 28px;
    |    margin: 5px 5px;
    |}
    |.mediuminput {
    |    width: 500px;
    |    font-size: 150%;
    |    margin: 2px 2px;
    |}
    |.radiobutton {
    |    font-size: 28px;
    |}
    |input[type='radio'] { transform: scale(1.5); }
    |input { font-size: 28px; }
    """.stripMargin

  def page(body: String, title: String): String = s"""<!DOCTYPE html>
    |<html><head><meta charset="UTF-8"><title>$title</title>
    |<style>
    |$styles
    |</style>
    |</head>
    |<body>
    |$body
    |</body>
    |</html>
    |""".stripMargin

  def h1(heading: String): String = s"<h1>$heading</h1>"

  def nl2br(soup: String): String = soup.replaceAllLiterally("\n"," </br>\n")    

  def helloPage(msg: String = "hello world!"): String = s"""|<!DOCTYPE html>
    |<html>
    |  <head>
    |    <title>Page Title</title>
    |    <style>
    |      body {background-color: pink;}
    |      h1 {color: red;}
    |      p {color: green;}
    |    </style>
    |  </head>
    |  <body>
    |    <h1>$msg</h1>
    |  </body>
    |</html>""".stripMargin

  def link(url: String, text: String = "here"): String = s"""<a href="$url">$text</a>"""

  def boldIf(cond: Boolean)(s: String) = if (cond) s"<b>$s</b>" else s
}