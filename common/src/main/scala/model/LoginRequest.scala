package sigrid.common.model

/** Request payload for user login.
  * @param name
  *   User's first name
  * @param course
  *   Course code (e.g., "PGK")
  * @param room
  *   Room name (e.g., "Hacke")
  * @param role
  *   User role as string ("student" or "supervisor")
  */
case class LoginRequest(
    name: String,
    course: String,
    room: String,
    role: String
)
