object html {
    val styles = """
    |body {
    |    background-color: pink;
    |    font-size: medium;
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
    |    font-size: large;
    |    margin: 8px 8px;
    |    cursor: pointer;
    |    font-family: "Lucida Console", Monaco, monospace;
    |}
    |.smallinput {
    |    width: 10%;
    |    font-size: medium;
    |    margin: 1px 1px;
    |}
    |.mediuminput {
    |    width: 20%;
    |    font-size: medium;
    |    margin: 1px 1px;
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
}