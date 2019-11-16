object html {
    val styles = """
    |body {
    |    background-color: pink;
    |    font-size: x-large;
    |    font-family: "Lucida Console", Monaco, monospace;
    |}
    |.button {
    |    background-color: #4CAF50;
    |    border: none;
    |    color: pink;
    |    padding: 8px 8px;
    |    text-align: center;
    |    text-decoration: none;
    |    display: inline-block;
    |    font-size: xx-large;
    |    margin: 8px 8px;
    |    cursor: pointer;
    |    font-family: "Lucida Console", Monaco, monospace;
    |}
    |.smallinput {
    |    width: 12%;
    |    font-size: x-large;
    |    margin: 2px 2px;
    |}
    |.mediuminput {
    |    width: 20%;
    |    font-size: x-large;
    |    margin: 2px 2px;
    |}
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
    |      p {color: blue;}
    |    </style>
    |  </head>
    |  <body>
    |    <h1>$msg</h1>
    |  </body>
    |</html>""".stripMargin

  def link(url: String, text: String = "here"): String = s"""<a href="$url">$text</a>"""

  def boldIf(cond: Boolean)(s: String) = if (cond) s"<b>$s</b>" else s
}