package sigrid.client.views

import com.raquo.laminar.api.L.*
import sigrid.client.state.UserStore
import scala.concurrent.ExecutionContext.Implicits.global

object Beppe:

  def apply(): HtmlElement =
    val isLoading = Var(false)
    val loginError = Var[Option[String]](None)

    mainTag(
      h1("Beppe - Login"),

      // Conditionally render login form or main content
      child <-- UserStore.isSupervisor.map({
        case false =>
          LoginForm(
            onSubmitCallback = (name, course, room) =>
              // TODO: Implement supervisor login logic
              ???,
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
