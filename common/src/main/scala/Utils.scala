package sigrid.common

object Utils:
  def reverseWords(text: String): String =
    text.split(" ").map(_.reverse).mkString(" ")

  def funkyCase(text: String): String =
    text.zipWithIndex.map { (char, index) =>
      if index % 2 == 0 then char.toLower else char.toUpper
    }.mkString
