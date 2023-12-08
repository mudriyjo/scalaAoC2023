package AdventOfCode.Day7

object Day7second {
  private val day: String = "day7"
  private val postfix: String = ".input"
  def parseLine(xs: String): List[Long] =
    xs.split(":").last.trim.split(" ").filter(_.nonEmpty).map(x => x.toLong).toList

  def execute(postfix: String) =
    val lines = AdventOfCode.readLine(day, postfix)
    println(lines)

  @main def mainSecond: Unit =
    println(execute(postfix))
}