package AdventOfCode.Day6

object Day6 {
  private val day: String = "day6"
  private val postfix: String = ".input"

  def parseLine(xs: String): List[Long] =
    xs.split(":").last.trim.split(" ").filter(_.nonEmpty).map(x => x.toLong).toList

  def timeToDistance(x: Long): List[Long] =
    (for i <- 0L to x
      yield {
        val movementTime = x - i
        val speed = i
        (movementTime * speed)
      }).toList
  def execute(postfix: String) =
    val lines = AdventOfCode.readLine(day, postfix)
    val times: List[Long] = parseLine(lines.head)
    val timesRange = times.map(x => timeToDistance(x)).toArray
    parseLine(lines.last).zipWithIndex.map{
      case (l, i) => timesRange(i).count(x => x > l)
    }.product

  @main def main: Unit =
    println(execute(postfix))
}