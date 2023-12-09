package AdventOfCode.Day8

object Day8second{
  private val day: String = "day8"
  private val postfix: String = ".input"

  def execute(postfix: String) =
    val lines = AdventOfCode.readLine(day, postfix)
    val res = for line <- lines yield line

  @main def mainSecond: Unit =
    println(execute(postfix))
}