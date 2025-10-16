package sigrid.client.network

import org.scalajs.dom
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.Thenable.Implicits.*
import scala.util.Try
import upickle.default.*
import sigrid.common.Serialization.given

object ApiClient:
  private val baseUrl = "http://localhost:8080/api"

  sealed trait ApiError
  case class NetworkError(message: String) extends ApiError
  case class HttpError(status: Int, message: String) extends ApiError
  case class ParseError(message: String) extends ApiError

  def get[T: ReadWriter](endpoint: String): Future[Either[ApiError, T]] =
    dom
      .fetch(s"$baseUrl$endpoint")
      .toFuture
      .flatMap(response =>
        if response.ok then
          response
            .text()
            .toFuture
            .map(jsonString =>
              Try(read[T](jsonString)).toEither.left.map(ex =>
                ParseError(s"Failed to parse JSON: ${ex.getMessage}")
              )
            )
        else
          response
            .text()
            .toFuture
            .map(errorText => Left(HttpError(response.status, errorText)))
      )
      .recover({ case ex =>
        Left(NetworkError(ex.getMessage))
      })

  def post[T: ReadWriter, B: ReadWriter](
      endpoint: String,
      requestBody: B
  ): Future[Either[ApiError, T]] =
    val bodyJson = write(requestBody)
    dom
      .fetch(
        s"$baseUrl$endpoint",
        new dom.RequestInit {
          method = dom.HttpMethod.POST
          headers = scalajs.js.Dictionary("Content-Type" -> "application/json")
          body = bodyJson
        }
      )
      .toFuture
      .flatMap(response =>
        if response.ok then
          response
            .text()
            .toFuture
            .map(jsonString =>
              Try(read[T](jsonString)).toEither.left.map(ex =>
                ParseError(s"Failed to parse JSON: ${ex.getMessage}")
              )
            )
        else
          response
            .text()
            .toFuture
            .map(errorText => Left(HttpError(response.status, errorText)))
      )
      .recover({ case ex =>
        Left(NetworkError(ex.getMessage))
      })
