package AdventOfCode.Day6

import scala.collection.parallel.CollectionConverters.*
object Day6second {
  private val day: String = "day6"
  private val postfix: String = ".input"

  def parseLine(xs: String): Long =
    xs.split(":").last.trim.split(" ").filter(_.nonEmpty).foldLeft("")((acc, x) => acc + x).toLong

  def timeToDistance(x: Long): List[Long] =
    (for i <- 0L to x
      yield {
        val movementTime = x - i
        val speed = i
        (movementTime * speed)
      }).toList

  def execute(postfix: String) =
    val lines = AdventOfCode.readLine(day, postfix)
    val timeRange = timeToDistance(parseLine(lines.head))
    val distance = parseLine(lines.last)
    timeRange.par.count(x => x > distance)

  @main def mainSecond: Unit =
    println(execute(postfix))
}