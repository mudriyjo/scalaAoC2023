package AdventOfCode.Day3

import scala.collection.mutable.ArrayBuffer
import scala.compiletime.ops.string
import scala.io.Source

object Day3 {

  private val day: String = "day3"
  private val postfix: String = ".input"

  def parseGame(x: String): String = x.filter(ch => ch.isDigit).head.toString

  def lineToMatrix(matrix: ArrayBuffer[Array[String]], line: String): ArrayBuffer[Array[String]] =
    matrix.addOne(line.split(""))

  def addToList(xs: List[(String, List[String], Boolean)], el: (String, List[String], Boolean)): List[(String, List[String], Boolean)] =
    if xs.isEmpty then List(el)
    else if xs.last._3 then xs.init :+ (xs.last._1 + el._1, xs.last._2 ++ el._2, xs.last._3)
    else xs :+ el

  def closeAdditionToNumber(xs: List[(String, List[String], Boolean)]): List[(String, List[String], Boolean)] =
    if xs.isEmpty then xs
    else xs.init :+ (xs.last._1, xs.last._2, false)

  def stringArround(matrix: ArrayBuffer[Array[String]], columnIndex: Int, rowIndex: Int): List[String] =
    val smallestColumn = if columnIndex - 1 >= 0 then columnIndex - 1 else 0
    val highestColumn = if columnIndex + 1 <= matrix.length - 1 then columnIndex + 1 else matrix.length - 1
    val smallestRow = if rowIndex - 1 >= 0 then rowIndex - 1 else 0
    val highestRow = if rowIndex + 1 <= matrix.head.length - 1 then rowIndex + 1 else matrix.head.length - 1
    val res = for
      columnI <- smallestColumn to highestColumn
      rowI <- smallestRow to highestRow
    yield matrix(rowI)(columnI)
    res.toList


  def findNumberAndAreaArround(matrix: ArrayBuffer[Array[String]]): List[(Int, List[String])] =
    matrix.zipWithIndex.foldLeft(List.empty[(String, List[String], Boolean)]) {
      case (acc, (line, rowIndex)) => {
        val res = line.zipWithIndex.foldLeft(List.empty[(String, List[String], Boolean)]) {
          case (acc, (element, columnIndex)) => {
            if element.charAt(0).isDigit then addToList(acc, (element, stringArround(matrix, columnIndex, rowIndex), true))
            else closeAdditionToNumber(acc)
          }
        }
        // TODO add logic for empty and non empty
        acc ++ closeAdditionToNumber(res)
      }
    }.map {
      case (str, value, bool) => (str.toInt, value)
    }

  def numsToCalculation(xs: List[(Int, List[String])]): List[Int] =
    xs.filter {
      case (num, value) => value.exists(x => !(x.charAt(0).isDigit || x == "."))
    }.map(x => x._1)

  def execute(postfix: String): Int = {
    val lines = AdventOfCode.readLine(day, postfix)
    val res = lines.foldLeft(ArrayBuffer.empty[Array[String]])((acc, text) => lineToMatrix(acc, text))
    numsToCalculation(findNumberAndAreaArround(res)).sum
  }

  @main def main: Unit =
    println(execute(postfix))
}