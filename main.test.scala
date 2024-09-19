package sigrid2

val port = "8080"
val host = s"http://localhost:$port"

class TestSuiteSigrid2 extends munit.FunSuite:
  override def beforeAll(): Unit =
    println("TODO: start server if not started")

  override def afterAll(): Unit =
    println("TODO: kill server if we started it in this test suite")

  test("GET hello"):
    val success = requests.get(host)
    assert(success.text().startsWith("Hello World!"))
    assert(success.statusCode == 200)

  test("POST reverse"):
    val success = requests.post(host + "/reverse", data = "hello")
    assert(success.text().startsWith("olleh"))

  test("POST add-all"):
    val pairs = Seq("a:b","c:d")
    val success = requests.post(host + "/add-all", data = pairs.mkString(";"))
    assert(success.text().startsWith(s"Added ${pairs.length}"))

  test("GET get-all"):
    val success = requests.get(host + "/get-all")
    println(s"GET get-all success.text()=${success.text()}")
    assert(success.text().startsWith(s"Map(a -> b, c -> d)"))


end TestSuiteSigrid2