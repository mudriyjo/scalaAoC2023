package AdventOfCode.Day8

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*

object Day8second{
  private val day: String = "day8"
  private val postfix: String = ".input"

  enum Move {
    case R, L
  }
  abstract class Graph {
    def getName: String
    def getLeft: Graph
    def getRight: Graph
  }

  object Graph {
    @tailrec
    final def findLastZ(move: List[Move], cntMove: Long, head: Graph): Long =
      if head.getName.split("").last == "Z" then cntMove
      else if move.head == Move.R then findLastZ(move.tail :+ move.head, cntMove + 1, head.getRight)
      else findLastZ(move.tail :+ move.head, cntMove + 1, head.getLeft)
  }

  class Node(val name: String, var left: Graph, var right: Graph) extends Graph {
    override def getName: String = name
    override def getLeft: Graph = left
    override def getRight: Graph = right
    override def toString = s"Node($name, ${left.getName}, ${right.getName})"
  }

  def wiring(xs: List[Node]) =
    xs.foreach(x => {
      val left = xs.find(y => y.name == x.left.getName)
      val right = xs.find(y => y.name == x.right.getName)
      x.left = left.get
      x.right = right.get
    })

  def strToLR(nodeTxt: String): (Node, Node) =
    val lr = nodeTxt.trim.replace("(", "").replace(")", "").split(",").toList
    val l = new Node(lr.head.trim, null, null)
    val r = new Node(lr.last.trim, null, null)
    if l.name == r.name then (l,l) else (l,r)
  def textToExec(xs: String): (List[Move], List[Graph]) =
    val txt = xs.split("\n").filter(x => x.nonEmpty).toList
    val move = txt.head.split("").map(x => Move.valueOf(x)).toList
    val graph = for node <- txt.tail yield {
      val nodeTxt = node.split("=")
      val name = nodeTxt.head.trim
      val (left,right) = strToLR(nodeTxt.last)
      new Node(name,left,right)
    }
    wiring(graph)
    (move, graph.filter(x => x.getName.split("").last == "A"))

  def gcd(a: Long, b: Long): Long =
    if (b == 0) a
    else gcd(b, a % b)

  def lcm(a: Long, b: Long): Long =
    if (a == 0 || b == 0) 0
    else math.abs(a * b) / gcd(a, b)

  def execute(postfix: String) =
    val lines = AdventOfCode.readLine(day, postfix)
    val (move, nodes) = textToExec(lines.mkString("\n"))
    val res =  nodes.par.map(node => Graph.findLastZ(move, 0, node))
    res.tail.fold(res.head)((acc, x) => lcm(acc, x))


  @main def mainSecond: Unit =
    println(execute(postfix))
}