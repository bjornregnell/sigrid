package sigrid.client.views

import com.raquo.laminar.api.L.*
import sigrid.client.network.ApiClient
import sigrid.client.state.UserStore
import sigrid.common.model.{User, Room, LoginRequest}
import sigrid.common.Serialization.given
import scala.concurrent.ExecutionContext.Implicits.global

object Sigrid:
  def apply(): HtmlElement =
    val isLoading = Var(false)
    val loginError = Var[Option[String]](None)

    mainTag(
      h1("Sigrid - Student Queue"),

      // Conditionally render login form or main content
      child <-- UserStore.isStudent.map({
        case false =>
          LoginForm(
            onSubmitCallback = (name, course, room) => {
              isLoading.set(true)
              loginError.set(None)

              val loginRequest = LoginRequest(name, course, room, "student")

              ApiClient
                .post[(User, Room), LoginRequest]("/login", loginRequest)
                .foreach {
                  case Right((user, room)) =>
                    println(s"Login successful! User: $user, Room: $room")
                    isLoading.set(false)
                    UserStore.login(user, room)
                    println(s"UserStore updated. isStudent should be true now.")
                  case Left(error) =>
                    println(s"Login failed with error: $error")
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
      h2("Welcome, Student!"),
      p("You are logged in as a student."),
      button(
        "Logout",
        onClick --> (_ => UserStore.logout())
      )
    )
