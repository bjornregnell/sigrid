package sigrid.client

import com.raquo.laminar.api.L.*
import org.scalajs.dom

@main def main(): Unit =
  render(
    // Where Laminar should render its tree
    container = dom.document.getElementById(
      "app"
    ),
    // What should be renderered
    rootNode = div(
      // Let the router manage children of root node
      child <-- Router.splitter.signal
    )
  )
