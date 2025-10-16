package sigrid.client.views

import com.raquo.laminar.api.L.*
import sigrid.client.network.ApiClient
import sigrid.client.state.UserStore
import sigrid.common.model.{User, Room, LoginRequest}
import sigrid.common.Serialization.given
import scala.concurrent.ExecutionContext.Implicits.global

object Beppe:

  def apply(): HtmlElement =
    val isLoading = Var(false)
    val loginError = Var[Option[String]](None)

    mainTag(
      h1("Beppe - Supervisor View"),

      // Conditionally render login form or main content
      child <-- UserStore.isSupervisor.map({
        case false =>
          LoginForm(
            onSubmitCallback = (name, course, room) => {
              isLoading.set(true)
              loginError.set(None)

              val loginRequest = LoginRequest(name, course, room, "supervisor")

              ApiClient
                .post[(User, Room), LoginRequest]("/login", loginRequest)
                .foreach {
                  case Right((user, room)) =>
                    isLoading.set(false)
                    UserStore.login(user, room)
                  case Left(error) =>
                    isLoading.set(false)
                    val errorMessage = error match
                      case ApiClient.NetworkError(msg) => s"Network error: $msg"
                      case ApiClient.HttpError(status, msg) =>
                        s"Server error ($status): $msg"
                      case ApiClient.ParseError(msg) => s"Parse error: $msg"
                    loginError.set(Some(errorMessage))
                }
            },
            loading = isLoading.signal,
            error = loginError.signal
          )
        case true => mainContent()
      })
    )

  private def mainContent(): HtmlElement =
    div(
      h2("Welcome, Supervisor!"),
      p("You are logged in as a supervisor."),
      button(
        "Logout",
        onClick --> (_ => UserStore.logout())
      )
    )
