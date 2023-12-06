package AdventOfCode.Day6

object Day6second {
  private val day: String = "day6"
  private val postfix: String = ".input"

  def execute(postfix: String) =
    val lines = AdventOfCode.readLine(day, postfix)
    println(lines)
    0

  @main def mainSecond: Unit =
    println(execute(postfix))
}