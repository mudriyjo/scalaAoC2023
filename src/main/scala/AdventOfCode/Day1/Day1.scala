package AdventOfCode.Day1

import scala.compiletime.ops.string

object Day1 {
  private val day: String = "day1"
  private val postfix: String = ".input"
  def firstDigit(x: String): String = x.filter(ch => ch.isDigit).head.toString

  def execute(postfix: String): Int = {
    val lines = AdventOfCode.readLine(day, postfix)
    val res = for line <- lines yield
      (firstDigit(line) + firstDigit(line.reverse)).toInt
    res.sum
  }
  @main def main: Unit =
    println(execute(postfix))
}