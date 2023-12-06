package AdventOfCode.Day5

import scala.collection.immutable.HashMap
object Day5 {
  private val day: String = "day5"
  private val postfix: String = ".input"

  def textToinput(xs: List[List[String]]): List[BigInt] =
    xs.head.head.split(":").last.trim.split(" ").map(x => BigInt(x)).toList

  def numToFn(xs: List[BigInt]): Map[(BigInt, BigInt), BigInt] =
    val range = xs.last - 1
    val destination = xs.head
    val source = xs.tail.head
    Map(((source, source + range), destination))

  def blockToFn(xs: List[String]): Map[String, (BigInt) => BigInt] =
    val name = xs.head.replace(" map:","")
    val map = xs.tail
      .map(x => numToFn(x.trim.split(" ").map(y => BigInt(y)).toList))
      .foldLeft(HashMap.empty[(BigInt, BigInt),BigInt])((acc, el) => acc ++ el)

    HashMap((name,(x: BigInt) => {
      map.foldLeft((x, false)) {
        case ((num, isApplied), ((sourceLeft, sourceRight), destination)) =>
          if isApplied then (num, isApplied)
          else if sourceLeft <= num && num <= sourceRight then ((num - sourceLeft + destination), true)
          else (num, isApplied)
      }._1
    }))

  def textToTransfer(xs: List[List[String]]): (List[BigInt], (BigInt) => BigInt) =
    val input = textToinput(xs)
    val mapBlock = xs.tail.foldLeft(HashMap.empty[String, (BigInt) => BigInt])((acc, x) => acc ++ blockToFn(x))
    val mapBlockToFn = mapBlock("seed-to-soil") andThen
        mapBlock("soil-to-fertilizer") andThen
        mapBlock("fertilizer-to-water") andThen
        mapBlock("water-to-light") andThen
        mapBlock("light-to-temperature") andThen
        mapBlock("temperature-to-humidity") andThen
        mapBlock("humidity-to-location")
    (input, mapBlockToFn)

  def textToResult(xs: List[List[String]]): List[BigInt] =
    val res = textToTransfer(xs)
      res._1.map(x => res._2(x))

  def groupBlocs(xs: List[String]): List[List[String]] =
    xs.foldLeft(List.empty[List[String]])((acc, el) => {
      if acc.nonEmpty && el.nonEmpty then acc.init :+ (acc.last :+ el)
      else if acc.nonEmpty && el.isEmpty then acc :+ List()
      else acc :+ List(el)
    })

  def execute(postfix: String) =
    val lines = AdventOfCode.readLine(day, postfix)
    textToResult(groupBlocs(lines)).min

  @main def main: Unit =
    println(execute(postfix))
}