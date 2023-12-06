package AdventOfCode.Day4

import AdventOfCode.Day4.Day4second.ScratchCard

import scala.collection.immutable.HashMap
import scala.compiletime.ops.string
import scala.io.Source

object Day4second {
  private val day: String = "day4"
  private val postfix: String = ".input"
  case class ScratchCard(num: Int, win: List[Int], card: List[Int], cardDuplicate: List[Int], var amount: Int = 1)

  def prepareScore(xs: String): List[Int] =
    xs.trim.split(" ").filter(x => x.nonEmpty).map(x => x.toInt).toList

  def textToCard(x: String): ScratchCard =
    val tmpText = x.split(":")
    val cardNum = tmpText.head.split(" ").last.toInt
    val winAndCard = tmpText.last.split("\\|")
    val win = prepareScore(winAndCard.head)
    val card = prepareScore(winAndCard.last)
    val amount = win.intersect(card).foldLeft(List.empty[Int])((acc, num) =>
      if acc.isEmpty then List(cardNum + 1)
      else acc :+ acc.last + 1
    )
    ScratchCard(cardNum, prepareScore(winAndCard.head), prepareScore(winAndCard.last), amount)

  def scratchCardToMap(xs: List[ScratchCard]): scala.collection.mutable.HashMap[Int, ScratchCard] =
    xs.foldLeft(scala.collection.mutable.HashMap.empty[Int, ScratchCard])((acc, x) => acc + (x.num -> x))

  def calculateAmountOfCard(xs: scala.collection.mutable.HashMap[Int, ScratchCard]): scala.collection.mutable.HashMap[Int, ScratchCard] =
    val res = xs
    xs.keys.foreach(key => {
      val dup = xs(key).cardDuplicate
      for dupCrd <- dup do {
        for crdIncr <- res.get(dupCrd) do crdIncr.amount += 1 * xs(key).amount
      }
    })
    res

  def sumScratchCard(xs: List[ScratchCard]): Long =
    xs.foldLeft(0)((acc, crd) => acc + crd.amount)

  def execute(postfix: String): Long = {
    val lines = AdventOfCode.readLine(day, postfix)
    val res = for line <- lines yield
      textToCard(line)

    sumScratchCard(calculateAmountOfCard(scratchCardToMap(res)).values.toList)
  }

  @main def mainSecond: Unit =
    println(execute(postfix))
}

