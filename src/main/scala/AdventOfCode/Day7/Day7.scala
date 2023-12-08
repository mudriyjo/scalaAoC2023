package AdventOfCode.Day7

import AdventOfCode.Day7.Day7.HandStrength.HighCard

object Day7 {
  private val day: String = "day7"
  private val postfix: String = ".input"

  enum Card {
    case Card2, Card3, Card4, Card5, Card6, Card7, Card8, Card9, T, J, Q, K, A
  }
  enum HandStrength {
    case HighCard, OnePair, TwoPair, ThreeOfAKind, FullHouse, FourOfAKind, FiveOfAKind
  }

  object Card {
    def apply(text: String): Card = {
      text match
        case "2" => Card2
        case "3" => Card3
        case "4" => Card4
        case "5" => Card5
        case "6" => Card6
        case "7" => Card7
        case "8" => Card8
        case "9" => Card9
        case str => Card.valueOf(str)
    }

    def handStrength(xs: List[Card]): HandStrength =
      val res = xs.groupBy(x => x).map(x => x._2.length).toList

      val kind = res.max match
        case 5 => Some(HandStrength.FiveOfAKind)
        case 4 => Some(HandStrength.FourOfAKind)
        case 3 => Some(HandStrength.ThreeOfAKind)
        case _ => None

      val fullHouse = res match
        case first :: second :: Nil => if first == 3 || first == 2 && second == 2 || second == 3 then Some(HandStrength.FullHouse) else None
        case xs => if xs.count(x => x == 2) == 2 then Some(HandStrength.TwoPair) else if xs.count(x => x == 2) == 1 then Some(HandStrength.OnePair) else None
        case _ => None
      fullHouse.orElse(kind).getOrElse(HighCard)

    def compareGameSameStrength(game1: Game, game2: Game): Int =
      game1.hand.zip(game2.hand).foldLeft(0)((acc, cards) =>
        if acc != 0 then acc
        else if cards._1.ordinal - cards._2.ordinal < 0 then 1
        else -1
      )
  }
  given Ordering[Game] with {
    def compare(game1: Game, game2: Game): Int =
      if game1.handStrength.ordinal != game2.handStrength.ordinal then game1.handStrength.ordinal.compare(game2.handStrength.ordinal)
      else game1.hand.zip(game2.hand).foldLeft(0)((acc, cards) =>
        if acc != 0 then acc
        else cards._1.ordinal.compare(cards._2.ordinal)
      )
  }
  case class Game(bid: Long, hand: List[Card], handStrength: HandStrength)

  def textToHand(text: String): List[Card] =
    text.split("").map(x => Card(x)).toList
  def textToGame(line: String): Game =
    val xs = line.split(" ")
    val hand = textToHand(xs.head)
    Game(xs.last.toLong, hand, Card.handStrength(hand))

  def parseLine(xs: String): List[Long] =
    xs.split(":").last.trim.split(" ").filter(_.nonEmpty).map(x => x.toLong).toList

  def execute(postfix: String) =
    val lines = AdventOfCode.readLine(day, postfix)
    val res = for line <- lines yield textToGame(line)
    res.foreach(println)
    res.sorted.zipWithIndex.map {
      case (game, i) =>  BigInt(game.bid) * BigInt((i + 1))
    }

  @main def main: Unit =
    println(execute(postfix).sum)
}