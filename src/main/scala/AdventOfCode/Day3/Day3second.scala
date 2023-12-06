package AdventOfCode.Day3

import scala.collection.mutable.ArrayBuffer
import scala.compiletime.ops.string
import scala.io.Source

object Day3second {

  private val day: String = "day3"
  private val postfix: String = ".input"

  def parseGame(x: String): String = x.filter(ch => ch.isDigit).head.toString

  def lineToMatrix(matrix: ArrayBuffer[Array[String]], line: String): ArrayBuffer[Array[String]] =
    matrix.addOne(line.split(""))

  def addToList(xs: List[(String, List[(String, (Int, Int))], Boolean)], el: (String, List[(String, (Int, Int))], Boolean)): List[(String, List[(String, (Int, Int))], Boolean)] =
    if xs.isEmpty then List(el)
    else if xs.last._3 then xs.init :+ (xs.last._1 + el._1, xs.last._2 ++ el._2, xs.last._3)
    else xs :+ el

  def closeAdditionToNumber(xs: List[(String, List[(String, (Int, Int))], Boolean)]): List[(String, List[(String, (Int, Int))], Boolean)] =
    if xs.isEmpty then xs
    else xs.init :+ (xs.last._1, xs.last._2, false)

  def stringArround(matrix: ArrayBuffer[Array[String]], columnIndex: Int, rowIndex: Int): List[(String, (Int, Int))] =
    val smallestColumn = if columnIndex - 1 >= 0 then columnIndex - 1 else 0
    val highestColumn = if columnIndex + 1 <= matrix.length - 1 then columnIndex + 1 else matrix.length - 1
    val smallestRow = if rowIndex - 1 >= 0 then rowIndex - 1 else 0
    val highestRow = if rowIndex + 1 <= matrix.head.length - 1 then rowIndex + 1 else matrix.head.length - 1
    val res = for
      columnI <- smallestColumn to highestColumn
      rowI <- smallestRow to highestRow
    yield (matrix(rowI)(columnI), (rowI, columnI))
    res.toList

  def filterNumberWithoutAsterisk(xs: List[(Int, List[(String, (Int, Int))])]): List[(Int, List[(String, (Int, Int))])] =
    xs.filter(el => el._2.exists(tpl => tpl._1 == "*"))

  def getNumByAsteriskIndexes(xs: List[(Int, List[(String, (Int, Int))])], indexes: (Int, Int)): Option[Int] =
    val res = xs.foldLeft(List.empty[Option[Int]]) {
      case (acc, (num, xs)) => {
        if xs.exists { case (str, tuple) => str == "*" && indexes == tuple } then acc :+ Some(num)
        else acc
      }
    }.filter(x => x.isDefined)
    if res.isEmpty then None
    else res.head

  def addToRes(xs: List[(Int, Int)], el: (Int, Int)): List[(Int, Int)] =
    if xs.exists { case (x, y) => (x == el._1 && y == el._2) || (x == el._2 && y == el._1) } then xs
    else xs :+ el

  def multiplyTwoNumberWithNearAsterisk(xs: List[(Int, List[(String, (Int, Int))])]): List[(Int, Int)] =
    xs.foldLeft(List.empty[(Int, Int)]) {
      case (acc, (num, list)) => {
        val searchXs = xs.filter(x => x._1 != num)
        val par = getNumByAsteriskIndexes(searchXs, list.filter(x => x._1 == "*").head._2)
        if par.isDefined then addToRes(acc, (num, par.get))
        else acc
      }
    }

  def findNumberAndAreaArround(matrix: ArrayBuffer[Array[String]]): List[(Int, List[(String, (Int, Int))])] =
    matrix.zipWithIndex.foldLeft(List.empty[(String, List[(String, (Int, Int))], Boolean)]) {
      case (acc, (line, rowIndex)) => {
        val res = line.zipWithIndex.foldLeft(List.empty[(String, List[(String, (Int, Int))], Boolean)]) {
          case (acc, (element, columnIndex)) => {
            if element.charAt(0).isDigit then addToList(acc, (element, stringArround(matrix, columnIndex, rowIndex), true))
            else {
              closeAdditionToNumber(acc)
            }
          }
        }
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
    val res1 = findNumberAndAreaArround(res)

    multiplyTwoNumberWithNearAsterisk(filterNumberWithoutAsterisk(res1)).map(x => x._2 * x._1).sum
  }

  @main def mainSecond: Unit =
    println(execute(postfix))
}