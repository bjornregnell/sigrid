package sigrid.client

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import sigrid.common.Utils
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.Thenable.Implicits.*

@main def main(): Unit =
  val message = "Hello World from Sigrid"
  val reversedWords = Utils.reverseWords(message)
  val funkyMessage = Utils.funkyCase("Welcome to the monorepo!")

  val pingResponseVar = Var("Loading...")

  // Fetch /ping from server
  dom
    .fetch("http://localhost:8080/ping")
    .toFuture
    .flatMap(_.text().toFuture)
    .foreach { response =>
      pingResponseVar.set(s"Server response: $response")
    }

  val app = div(
    h1(message),
    p(s"Reversed words: $reversedWords"),
    p(s"Funky case: $funkyMessage"),
    p(child.text <-- pingResponseVar.signal)
  )

  render(dom.document.getElementById("app"), app)
