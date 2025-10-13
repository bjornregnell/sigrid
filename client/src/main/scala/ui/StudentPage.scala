package sigrid.client.ui

import com.raquo.laminar.api.L.*
import sigrid.client.network.ApiClient
import scala.concurrent.ExecutionContext.Implicits.global

object StudentPage:
  private val endpoint = Var("/ping")
  private val result = Var("")

  private def makeRequest(): Unit =
    ApiClient
      .get(endpoint.now())
      .foreach({
        case Right(data) => result.set(s"Success: $data")
        case Left(error) => result.set(s"Error: $error")
      })

  def apply(): HtmlElement =
    mainTag(
      input(
        typ := "text",
        value <-- endpoint.signal,
        onInput.mapToValue --> endpoint
      ),
      button(
        "Make Request",
        onClick --> (_ => makeRequest())
      ),
      p(text <-- result.signal)
    )
