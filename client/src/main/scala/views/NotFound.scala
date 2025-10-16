package sigrid.client.views

import com.raquo.laminar.api.L.*
import com.raquo.waypoint.Router
import sigrid.client.Page
import sigrid.client.StudentPage

object NotFound:
  def apply(router: Router[Page], path: String): HtmlElement =
    div(
      h1("404 - Sidan finns inte"),
      a(
        router.navigateTo(StudentPage),
        "Tillbaka till sigrid"
      )
    )
