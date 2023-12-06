import scala.io.Source
package object AdventOfCode {
  def filePath(day: String): String = s"/Users/Alexander_Babin/project/scala/aoc2023/input/${day}_inputs/"

  def readLine(day: String, postfix: String = ""): List[String] =
    val resource = Source.fromFile(filePath(day) + day + postfix)
    val lines = resource.getLines().toList
    resource.close()
    lines
}
