package sigrid.client

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import ui.{StudentPage, SupervisorPage}

@main def main(): Unit =
  render(
    dom.document.getElementById("app"),
    div(
      headerTag("Header"),
      StudentPage(),
      SupervisorPage()
    )
  )
