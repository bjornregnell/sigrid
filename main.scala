package sigrid2

val db: storky.Store[String, String] = storky.Store.empty()

object MinimalApplication extends cask.MainRoutes:
  
  @cask.get("/")
  def hello() = 
    println("Server GET /")
    "Hello World!\n"

  @cask.get("/get-all")
  def getAll() = 
    println("Server GET /get-all")
    db.toMap.toString

  @cask.post("/reverse")
  def doThing(request: cask.Request) = 
    println(s"Server POST /do-thing text=${request.text()}")
    request.text().reverse + "\n"

  @cask.post("/add-all")
  def add(request: cask.Request) = 
    println(s"Server POST /put text=${request.text()}")
    val kvs = request.text().split(";").toSeq.map(_.split(":").toSeq)
    for case Seq(k, v) <- kvs do db.put(k, v)
    s"Added ${kvs.length} pairs"

  initialize()

  println("server started at port=" + port)
  println("debugMode=" + debugMode)
  println("verbose=" + verbose)
  println("Press Ctrl+C to kill server...")
  
