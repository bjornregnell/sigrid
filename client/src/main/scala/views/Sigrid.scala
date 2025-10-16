package sigrid.client.views

import com.raquo.laminar.api.L.*
import sigrid.client.network.ApiClient
import sigrid.client.state.UserStore
import scala.concurrent.ExecutionContext.Implicits.global

object Sigrid:
  def apply(): HtmlElement =
    val isLoading = Var(false)
    val loginError = Var[Option[String]](None)

    mainTag(
      h1("Sigrid - Login"),

      // Conditionally render login form or main content
      child <-- UserStore.isStudent.map({
        case false =>
          LoginForm(
            onSubmitCallback = (name, course, room) =>
              // TODO: Implement student login logic
              ???,
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
