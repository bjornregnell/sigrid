package sigrid.client.network

import org.scalajs.dom
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.Thenable.Implicits.*

object ApiClient:
  private val baseUrl = "http://localhost:8080"

  sealed trait ApiError
  case class NetworkError(message: String) extends ApiError
  case class HttpError(status: Int, message: String) extends ApiError
  case class ParseError(message: String) extends ApiError

  def get(endpoint: String): Future[Either[ApiError, String]] =
    dom
      .fetch(s"$baseUrl$endpoint")
      .toFuture
      .flatMap(response =>
        if response.ok then response.text().toFuture.map(Right(_))
        else
          response
            .text()
            .toFuture
            .map(errorText => Left(HttpError(response.status, errorText)))
      )
      .recover({ case ex =>
        Left(NetworkError(ex.getMessage))
      })

  def post(
      endpoint: String,
      requestBody: String
  ): Future[Either[ApiError, String]] =
    val requestHeaders = new dom.Headers()
    requestHeaders.set("Content-Type", "application/json")

    val requestInit = new dom.RequestInit {
      method = dom.HttpMethod.POST
      headers = requestHeaders
      body = requestBody
    }

    dom
      .fetch(s"$baseUrl$endpoint", requestInit)
      .toFuture
      .flatMap(response =>
        if response.ok then response.text().toFuture.map(Right(_))
        else
          response
            .text()
            .toFuture
            .map(errorText => Left(HttpError(response.status, errorText)))
      )
      .recover({ case ex =>
        Left(NetworkError(ex.getMessage))
      })
