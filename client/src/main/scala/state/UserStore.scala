package sigrid.client.state

import com.raquo.laminar.api.L.*
import sigrid.common.model.{User, Room}
import sigrid.common.model.Role

object UserStore:
  // Internal mutable state
  private val userVar = Var[Option[User]](None)
  private val roomVar = Var[Option[Room]](None)

  // Public read-only signals which UI can react to
  val userSignal: Signal[Option[User]] = userVar.signal
  val roomSignal: Signal[Option[Room]] = roomVar.signal
  val isLoggedIn: Signal[Boolean] = userSignal.map(_.isDefined)
  val isStudent: Signal[Boolean] =
    userSignal.map(_.exists(_.role == Role.Student))
  val isSupervisor: Signal[Boolean] = userSignal.map(
    _.exists(
      _.role ==
        Role.Supervisor
    )
  )

  def login(user: User, room: Room): Unit =
    userVar.set(Some(user))
    roomVar.set(Some(room))

  def logout(): Unit =
    userVar.set(None)
    roomVar.set(None)
