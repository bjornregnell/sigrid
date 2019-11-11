
object Main {
  val ServerPort = 8091
  val ServerIP   = "0.0.0.0"
  
  def main(args: Array[String]): Unit = {
    import scala.util.Try
    SigridServer.start(
      host = args.lift(0).getOrElse(ServerIP),
      port = Try(args(1).toInt).getOrElse(ServerPort)
    )
  }
}