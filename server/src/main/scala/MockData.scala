package sigrid.server

import sigrid.common.model.{Room, User, Role, RoomKey}

object MockData:
  private val now = System.currentTimeMillis()
  private val oneMinuteAgo = now - (1 * 60 * 1000)
  private val twoMinutesAgo = now - (2 * 60 * 1000)
  private val threeMinutesAgo = now - (3 * 60 * 1000)
  private val fiveMinutesAgo = now - (5 * 60 * 1000)
  private val sevenMinutesAgo = now - (7 * 60 * 1000)
  private val tenMinutesAgo = now - (10 * 60 * 1000)
  private val fifteenMinutesAgo = now - (15 * 60 * 1000)

  // Supervisors
  val gustav = User("gustav", 1, Role.Supervisor)
  val helena = User("helena", 1, Role.Supervisor)
  val ingrid = User("ingrid", 1, Role.Supervisor)
  val johan = User("johan", 1, Role.Supervisor)

  // Students
  val alice = User("alice", 1, Role.Student)
  val bob = User("bob", 1, Role.Student)
  val charlie = User("charlie", 1, Role.Student)
  val diana = User("diana", 1, Role.Student)
  val erik = User("erik", 1, Role.Student)
  val fiona = User("fiona", 1, Role.Student)
  val george = User("george", 1, Role.Student)
  val hanna = User("hanna", 1, Role.Student)
  val ivan = User("ivan", 1, Role.Student)
  val julia = User("julia", 1, Role.Student)
  val kalle = User("kalle", 1, Role.Student)
  val lisa = User("lisa", 1, Role.Student)
  val martin = User("martin", 1, Role.Student)
  val nina = User("nina", 1, Role.Student)
  val oliver = User("oliver", 1, Role.Student)
  val petra = User("petra", 1, Role.Student)
  val quinn = User("quinn", 1, Role.Student)
  val rita = User("rita", 1, Role.Student)
  val sven = User("sven", 1, Role.Student)
  val tina = User("tina", 1, Role.Student)
  val urban = User("urban", 1, Role.Student)
  val vera = User("vera", 1, Role.Student)
  val wilhelm = User("wilhelm", 1, Role.Student)
  val xander = User("xander", 1, Role.Student)
  val yvonne = User("yvonne", 1, Role.Student)
  val zara = User("zara", 1, Role.Student)
  val adam = User("adam", 1, Role.Student)
  val beatrice = User("beatrice", 1, Role.Student)
  val cecil = User("cecil", 1, Role.Student)

  // Room Alfa: High load, 2 supervisors, 10 students, 3 in help queue, 2 in approval queue
  val roomAlfa = Room(
    course = "PGK",
    name = "Alfa",
    users = Set(gustav, helena, alice, bob, charlie, diana, erik, ivan, julia, urban, vera, wilhelm),
    helpQueue = Vector(
      (alice, fifteenMinutesAgo),
      (charlie, tenMinutesAgo),
      (diana, fiveMinutesAgo)
    ),
    approvalQueue = Vector(
      (bob, sevenMinutesAgo),
      (erik, threeMinutesAgo)
    ),
    created = now
  )

  // Room Hacke: Medium load, 1 supervisor, 5 students, 1 in help queue
  val roomHacke = Room(
    course = "PGK",
    name = "Hacke",
    users = Set(ingrid, kalle, lisa, martin, nina, oliver),
    helpQueue = Vector(
      (kalle, fiveMinutesAgo)
    ),
    approvalQueue = Vector.empty,
    created = now
  )

  // Room Gamma: Very light load, no supervisor, 2 students, no queues
  val roomGamma = Room(
    course = "PGK",
    name = "Gamma",
    users = Set(petra, quinn),
    helpQueue = Vector.empty,
    approvalQueue = Vector.empty,
    created = now
  )

  // Room Elgkalv: Average load, 1 supervisor, 6 students, 1 in help queue, 2 in approval queue
  val roomElgkalv = Room(
    course = "PGK",
    name = "Elgkalv",
    users = Set(johan, rita, sven, tina, fiona, george, hanna),
    helpQueue = Vector(
      (rita, sevenMinutesAgo)
    ),
    approvalQueue = Vector(
      (sven, twoMinutesAgo),
      (tina, oneMinuteAgo)
    ),
    created = now
  )

  // Room Beta: Light load, 1 supervisor, 3 students, 1 in help queue
  val roomBeta = Room(
    course = "PGK",
    name = "Beta",
    users = Set(helena, xander, yvonne, zara),
    helpQueue = Vector(
      (xander, threeMinutesAgo)
    ),
    approvalQueue = Vector.empty,
    created = now
  )

  // Room Jupiter: Medium load, 1 supervisor, 4 students, 2 in approval queue
  val roomJupiter = Room(
    course = "PGK",
    name = "Jupiter",
    users = Set(ingrid, adam, beatrice, cecil, quinn),
    helpQueue = Vector.empty,
    approvalQueue = Vector(
      (adam, fiveMinutesAgo),
      (beatrice, twoMinutesAgo)
    ),
    created = now
  )

  // Room E:2313: Low load, no supervisor, 2 students, no queues (uncategorized room)
  val roomE2313 = Room(
    course = "PGK",
    name = "E:2313",
    users = Set(oliver, martin),
    helpQueue = Vector.empty,
    approvalQueue = Vector.empty,
    created = now
  )

  val rooms: Map[RoomKey, Room] = Map(
    RoomKey("PGK", "Alfa") -> roomAlfa,
    RoomKey("PGK", "Hacke") -> roomHacke,
    RoomKey("PGK", "Gamma") -> roomGamma,
    RoomKey("PGK", "Elgkalv") -> roomElgkalv,
    RoomKey("PGK", "Beta") -> roomBeta,
    RoomKey("PGK", "Jupiter") -> roomJupiter,
    RoomKey("PGK", "E:2313") -> roomE2313
  )

  val userNames: Map[String, Vector[Int]] = Map(
    "gustav" -> Vector(1),
    "helena" -> Vector(1),
    "ingrid" -> Vector(1),
    "johan" -> Vector(1),
    "alice" -> Vector(1),
    "bob" -> Vector(1),
    "charlie" -> Vector(1),
    "diana" -> Vector(1),
    "erik" -> Vector(1),
    "fiona" -> Vector(1),
    "george" -> Vector(1),
    "hanna" -> Vector(1),
    "ivan" -> Vector(1),
    "julia" -> Vector(1),
    "kalle" -> Vector(1),
    "lisa" -> Vector(1),
    "martin" -> Vector(1),
    "nina" -> Vector(1),
    "oliver" -> Vector(1),
    "petra" -> Vector(1),
    "quinn" -> Vector(1),
    "rita" -> Vector(1),
    "sven" -> Vector(1),
    "tina" -> Vector(1),
    "urban" -> Vector(1),
    "vera" -> Vector(1),
    "wilhelm" -> Vector(1),
    "xander" -> Vector(1),
    "yvonne" -> Vector(1),
    "zara" -> Vector(1),
    "adam" -> Vector(1),
    "beatrice" -> Vector(1),
    "cecil" -> Vector(1)
  )
