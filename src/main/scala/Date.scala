import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

case class Date(year: Int, month: Int, dayOfMonth: Int, 
                hour: Int, minute: Int, second: Int
                ) extends Ordered[Date] {

  val calendar = LocalDateTime.of(year, month, dayOfMonth, hour, minute, second)

  def toFormat(format: String): String = calendar.format(DateTimeFormatter.ofPattern(format))

  lazy val show: String = toFormat("yyyy-MM-dd HH:mm:ss")

  def compare(other: Date): Int = calendar.compareTo(other.calendar)
}

object Date {
  def now(): Date = {
    import LocalDateTime.{now => j} 
    Date(j.getYear, j.getMonthValue, j.getDayOfMonth, j.getHour, j.getMinute, j.getSecond)
  }
}
