package sigrid.client.views

import com.raquo.laminar.api.L.*
import sigrid.client.network.ApiClient
import scala.concurrent.ExecutionContext.Implicits.global

object Student:

  def apply(): HtmlElement =
    mainTag(
      "sigrid"
    )
