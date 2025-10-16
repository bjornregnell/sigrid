package sigrid.client.views

import com.raquo.laminar.api.L.*

object LoginForm:

  def apply(
      onSubmitCallback: (name: String, course: String, room: String) => Unit,
      loading: Signal[Boolean],
      error: Signal[Option[String]]
  ): HtmlElement =
    val nameInput = Var("")
    val courseInput = Var("")
    val roomInput = Var("")

    form(
      onSubmit.preventDefault --> ({ _ =>
        onSubmitCallback(
          name = nameInput.now(),
          course = courseInput.now(),
          room = roomInput.now()
        )
      }),
      input(
        placeholder := "Name",
        controlled(
          value <-- nameInput.signal,
          onInput.mapToValue --> nameInput
        ),
        disabled <-- loading
      ),
      input(
        placeholder := "Course",
        controlled(
          value <-- courseInput.signal,
          onInput.mapToValue --> courseInput
        ),
        disabled <-- loading
      ),
      input(
        placeholder := "Rum",
        controlled(
          value <-- roomInput.signal,
          onInput.mapToValue --> roomInput
        ),
        disabled <-- loading
      ),
      button(
        typ := "submit",
        "Logga in",
        disabled <-- loading
      )
    )
