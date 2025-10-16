package sigrid.common.model

import scala.util.Try

enum Role:
  case Student, Supervisor

case class User(name: String, number: Int, role: Role):
  require(name == User.validName(name), s"invalid user name: $name")
  require(number > 0, s"invalid user number: $number")
  val id = s"$name-$number-${role.toString.toLowerCase}"
  override def toString = id

object User:
  val DefaultUserName = "oddput"
  val MaxNameLength = 25

  def validName(name: String): String =
    if (name.nonEmpty) name.filter(_.isLetter).take(MaxNameLength).toLowerCase
    else DefaultUserName

  def validUserId(id: String): String =
    id.filter(c => c.isLetterOrDigit || c == '-').toLowerCase

  def fromUserId(uid: String): Option[User] = Try({
    val xs = validUserId(uid).split('-')
    assert(xs.length == 3)
    val role = xs(2) match
      case "student"    => Role.Student
      case "supervisor" => Role.Supervisor
      case _ => throw new IllegalArgumentException(s"Invalid role: ${xs(2)}")
    User(validName(xs(0)), xs(1).toInt, role)
  }).toOption
