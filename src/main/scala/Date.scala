import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

case class Date(year: Int, month: Int, dayOfMonth: Int, 
                hour: Int, minute: Int, second: Int
                ) extends Ordered[Date] {

  val dateTime = LocalDateTime.of(year, month, dayOfMonth, hour, minute, second)

  def toFormat(format: String): String = dateTime.format(DateTimeFormatter.ofPattern(format))

  lazy val show: String = toFormat("yyyy-MM-dd HH:mm:ss")

  def compare(other: Date): Int = dateTime.compareTo(other.dateTime)

  def minusHours(h: Long): Date = Date(dateTime.minusHours(h))
  
}

object Date {
  def now(): Date = apply(LocalDateTime.now)

  def apply(dateTime: LocalDateTime): Date = {
    val j = dateTime
    Date(j.getYear, j.getMonthValue, j.getDayOfMonth, j.getHour, j.getMinute, j.getSecond)
  }
}
