package sigrid.client

import com.raquo.laminar.api.L.*
import com.raquo.waypoint.*
import urldsl.vocabulary.UrlMatching
import upickle.default.{ReadWriter, macroRW}

import sigrid.client.views

sealed abstract class Page(val title: String) derives ReadWriter
case object StudentPage extends Page("Sigrid")
case object SupervisorPage extends Page("Beppe")
case object MonitorPage extends Page("Monitor")
case class NotFoundPage(path: String) extends Page("Sigrid - 404")

object Router:
  // http://sigrid.example
  val sigridRoute = Route.static(StudentPage, root / endOfSegments)

  // http://sigrid.example/beppe
  val beppeRoute =
    Route.static(SupervisorPage, root / "beppe" / endOfSegments)

  // http://sigrid.example/monitor
  val monitorRoute =
    Route.static(MonitorPage, root / "monitor" / endOfSegments)

  private object RouterInstance
      extends Router[Page](
        routes = List(
          sigridRoute,
          beppeRoute,
          monitorRoute
        ),
        getPageTitle = _.title,
        serializePage = page => upickle.default.write(page),
        deserializePage = pageStr => upickle.default.read[Page](pageStr),
        routeFallback = path => NotFoundPage(path)
      )

  val splitter =
    SplitRender[Page, HtmlElement](RouterInstance.currentPageSignal)
      .collectStatic(StudentPage)(views.Sigrid())
      .collectStatic(SupervisorPage)(views.Beppe())
      .collectStatic(MonitorPage)(views.Monitor())
      .collect[NotFoundPage]({ case page =>
        views.NotFound(RouterInstance, page.path)
      })
