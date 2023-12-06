package AdventOfCode.Day4

import scala.compiletime.ops.string

object Day4 {
  private val day: String = "day4"
  private val postfix: String = ".input"

  case class Card(num: Int, win: List[Int], card: List[Int])

  def prepareScore(xs: String):List[Int] =
    xs.trim.split(" ").filter(x => x.nonEmpty).map(x => x.toInt).toList
  def textToCard(x: String): Card =
    val tmpText = x.split(":")
    val cardNum = tmpText.head.split(" ").last.toInt
    val winAndCard = tmpText.last.split("\\|")
    Card(cardNum, prepareScore(winAndCard.head), prepareScore(winAndCard.last))

  def winCard(c: Card): List[Int] =
    c.win.intersect(c.card)

  def calculateResult(xs: List[Int]): Int =
    if xs.isEmpty then 0
    else scala.math.pow(2, xs.length - 1).toInt
  def execute(postfix: String): Int = {
    val lines = AdventOfCode.readLine(day, postfix)
    val res = for line <- lines yield
      calculateResult(winCard(textToCard(line)))

    res.sum

  }
  @main def main: Unit =
    println(execute(postfix))
}