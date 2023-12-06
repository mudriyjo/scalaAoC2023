package AdventOfCode.Day2

import scala.collection.mutable
import scala.io.Source

object Day2second {
  private val day: String = "day2"
  private val postfix: String = ".input"

  object Cube {
    def apply(cube: String): Cube =
      val tmpCube = cube.trim.split(" ")
      val num = tmpCube.head.toInt
      tmpCube.last match
        case "blue" => Blue(num)
        case "red" => Red(num)
        case "green" => Green(num)


    def cubeToString(cube: Cube): String =
      cube match
        case Cube.Red(_) => "red"
        case Cube.Green(_) => "green"
        case Cube.Blue(_) => "blue"

    def highest(x: Cube, y: Cube): Cube = (x, y) match
      case (Blue(x1), Blue(x2)) => if x1 > x2 then x else y
      case (Red(x1), Red(x2)) => if x1 > x2 then x else y
      case (Green(x1), Green(x2)) => if x1 > x2 then x else y
      case _ => x

    def contains(xs: Set[Cube], cube: Cube): Boolean =
      xs.map(x => Cube.cubeToString(x)).contains(cubeToString(cube))
  }

  enum Cube(val num: Int) {
    case Red(val count: Int) extends Cube(count)
    case Green(val count: Int) extends Cube(count)
    case Blue(val count: Int) extends Cube(count)
  }

  case class Game(id: Int, cubes: List[Set[Cube]])

  def textToGame(x: String): Game =
    val tmpText = x.split(":")
    val gameNum = tmpText.head.split(" ").last.toInt
    val cubeSet = tmpText.last.split(";").map(x => {
      x.split(',').map(textCube => Cube.apply(textCube)).toSet
    }).toList
    Game(gameNum, cubeSet)

  def minimumSet(xs: Game): Set[Cube] =
    xs.cubes.foldLeft(Set[Cube]())((acc, cubeSet) =>
      cubeSet.foldLeft(acc)((accInner, c) => {
        if accInner.isEmpty || !Cube.contains(accInner, c) then accInner + c
        else accInner.map(innerC => Cube.highest(innerC, c))
      })
    )

  def parseGame(x: String): String = x.filter(ch => ch.isDigit).head.toString

  def execute(postfix: String): Int = {
    val lines = AdventOfCode.readLine(day, postfix)
    val res = for line <- lines yield
      val game = textToGame(line)
      minimumSet(game).toList.map(x => x.num).product

    res.sum
  }
  @main def mainSecond: Unit =
    println(execute(postfix))
}