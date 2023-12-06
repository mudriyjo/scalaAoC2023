package AdventOfCode.Day2

import scala.compiletime.ops.string
import scala.io.Source

object Day2 {
  private val day: String = "day2"
  private val postfix: String = ".input"

  object Cube {
    def apply(cube: String): Cube = {
      val tmpCube = cube.trim.split(" ")
      val num = tmpCube.head.toInt
      tmpCube.last match
        case "blue" => RED(num)
        case "red" => GREEN(num)
        case "green" => BLUE(num)
        case _ => throw Error(s"parsing error, data: ${tmpCube.mkString}")
    }

    def valid(cube: Cube, cnd: (Cube) => Boolean): Boolean =
      cnd(cube)
  }

  enum Cube(num: Int) {
    case RED(count: Int) extends Cube(count)
    case GREEN(count: Int) extends Cube(count)
    case BLUE(count: Int) extends Cube(count)
  }

  case class Game(id: Int, cubes: List[Set[Cube]])

  def textToGame(x: String): Game =
    val tmpText = x.split(":")
    val gameNum = tmpText.head.split(" ").last.toInt
    val cubeSet = tmpText.last.split(";").map(x => {
      x.split(',').map(textCube => Cube.apply(textCube)).toSet
    }).toList
    Game(gameNum, cubeSet)

  def validGame(cnd: (Set[Cube]) => Boolean, xs: Game): Option[Game] =
    if xs.cubes.forall(x => cnd(x)) then Some(xs) else None

  def parseGame(x: String): String = x.filter(ch => ch.isDigit).head.toString

  def execute(postfix: String): Int = {
    val lines = AdventOfCode.readLine(day, ".input")
    val res = for line <- lines yield
      val game = textToGame(line)
      validGame((x) => x.forall {
        case Cube.BLUE(num) => num <= 14
        case Cube.RED(num) => num <= 12
        case Cube.GREEN(num) => num <= 13
      }, game)

    val winGames = for
      game <- res
      gm <- game
    yield gm.id

    winGames.sum
  }

  @main def main: Unit =
    println(execute(postfix))
}