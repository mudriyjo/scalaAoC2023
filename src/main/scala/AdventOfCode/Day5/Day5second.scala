package AdventOfCode.Day5

import scala.collection.immutable.HashMap
import scala.collection.parallel.CollectionConverters.*
object Day5second {
  private val day: String = "day5"
  private val postfix: String = ".input"

  case class Input(from: Long, to: Long)
  def textToInput(xs: List[List[String]]): List[Input] =
    xs.head.head.split(":").last.trim.split(" ")
      .grouped(2).map(x => Input(x.head.toLong, x.head.toLong + x.last.toLong)).toList

  def numToFn(xs: List[Long]): Map[(Long, Long), Long] =
    val range = xs.last - 1
    val destination = xs.head
    val source = xs.tail.head
    val mapTable = Map(((source, source + range), destination))
    mapTable

  def blockToFn(xs: List[String]): Map[String, Long => Long] =
    val name = xs.head.replace(" map:", "")
    val map = xs.tail
      .map(x => numToFn(x.trim.split(" ").map(y => y.toLong).toList))
      .foldLeft(HashMap.empty[(Long, Long), Long])((acc, el) => acc ++ el)

    HashMap((name, (x: Long) => {
      map.find{case ((sourceLeft, sourceRight), destination) => sourceLeft <= x && x <= sourceRight } match {
        case Some((sourceLeft, sourceRight), destination) => (x - sourceLeft + destination)
        case None => x
      }
    }))

  def textToTransfer(xs: List[List[String]]): (List[Input], (Long) => Long) =
    val input = textToInput(xs)
    val mapBlock = xs.tail.foldLeft(HashMap.empty[String, (Long) => Long])((acc, x) => acc ++ blockToFn(x))
    val mapBlockToFn = mapBlock("seed-to-soil") andThen
      mapBlock("soil-to-fertilizer") andThen
      mapBlock("fertilizer-to-water") andThen
      mapBlock("water-to-light") andThen
      mapBlock("light-to-temperature") andThen
      mapBlock("temperature-to-humidity") andThen
      mapBlock("humidity-to-location")
    (input, mapBlockToFn)

  def textToResult(xs: List[List[String]]): Long =
    val res = textToTransfer(xs)
    val tmp = res._1.zipWithIndex
    val rx = Array.fill(10)(Long.MaxValue)
    for {
      (input, i) <- tmp.par
    } do {
        println(s"seed number: ${i}")
        for y <- input.from until input.to do if rx(i) > res._2(y) then rx(i) = res._2(y)
    }
    rx.foldLeft(Long.MaxValue)((acc, el) => if acc > el then el else acc)


  def groupBlocs(xs: List[String]): List[List[String]] =
    xs.foldLeft(List.empty[List[String]])((acc, el) => {
      if acc.nonEmpty && el.nonEmpty then acc.init :+ (acc.last :+ el)
      else if acc.nonEmpty && el.isEmpty then acc :+ List()
      else acc :+ List(el)
    })

  def execute(postfix: String) =
    val lines = AdventOfCode.readLine(day, postfix)
    textToResult(groupBlocs(lines))

  @main def mainSecond: Unit =
    println(execute(postfix))
}

