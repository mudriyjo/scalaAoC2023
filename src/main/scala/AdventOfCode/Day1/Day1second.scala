package AdventOfCode.Day1

import scala.collection.immutable.HashMap
import scala.compiletime.ops.string
import scala.io.Source

object Day1second {
  private val day: String = "day1"
  private val postfix: String = ".input"

  val numMapping: HashMap[String, String] = HashMap(
    "one" -> "1",
    "two" -> "2",
    "three" -> "3",
    "four" -> "4",
    "five" -> "5",
    "six" -> "6",
    "seven" -> "7",
    "eight" -> "8",
    "nine" -> "9",
  )


  def strToNum(x: String): String = numMapping.foldLeft(x) {
    case (acc, (name, value)) =>
      if acc.contains(name) then
        acc.replace(name, value) + name.last
      else
        acc
  }

  def mapping(x: String): String = x.foldLeft("") { case (acc, ch) => strToNum(acc + ch) }

  def firstLastDigit(x: String): String =
    val digits = x.filter(ch => ch.isDigit)
    val firstDigit = digits.head.toString
    val lastDigit = digits.reverse.head.toString
    firstDigit + lastDigit

  def execute(postfix: String): Int = {
    val lines = AdventOfCode.readLine(day, postfix)
    val res = for line <- lines yield
      val conv = mapping(line)
      firstLastDigit(conv).toInt

    res.sum
  }

  @main def mainSecond: Unit =
    println(execute(postfix))
}

