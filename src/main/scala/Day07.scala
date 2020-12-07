import scala.annotation.tailrec
import scala.io.Source
import scala.util.matching.Regex

trait Day07 {
  lazy val data: LazyList[String] =
    Source
      .fromInputStream(getClass.getResourceAsStream("input_07.txt"))
      .getLines()
      .map(_.trim)
      .to(LazyList)

  val lineRegex: Regex       = """([\w ]+) bags contain (.*)\.""".r
  val childrenRegex: Regex   = """(\d+) ([\w ]+) bag[s]?""".r
  val noChildrenRegex: Regex = """no other bags""".r

  type Color = String

  case class BagInfo(amount: Int, color: Color)

  val bagMap: Map[BagInfo, List[BagInfo]] =
    data.map {
      case lineRegex(parentColor, children) =>
        (
          BagInfo(1, parentColor),
          children.split(", ").toList.flatMap {
            case childrenRegex(amount, color) => Some(BagInfo(amount.toInt, color))
            case noChildrenRegex()            => None
          }
        )
    }.toMap
}

object Day07_1 extends App with Day07 {
  val byColor = bagMap.map { case (k: BagInfo, v: List[BagInfo]) => (k.color, v.map(_.color).toSet) }

  def findParents(color: Color): Set[Color] = {
    @tailrec
    def find0(colors: Set[Color], data: Map[Color, Set[Color]], acc: Set[Color]): Set[Color] = {
      val parentColors = data.filter { case (_: Color, v: Set[Color]) => v.exists(colors.contains) }.keySet
      val newColors    = parentColors -- acc
      if (newColors.isEmpty) acc else find0(newColors, data, parentColors ++ acc)
    }

    find0(Set(color), byColor, Set())
  }

  println(findParents("shiny gold").size)
}

object Day07_2 extends App with Day07 {
  case class Node(parent: BagInfo, children: List[Node]) {
    def amount: Int = parent.amount + children.map(_.amount).sum
  }

  def buildTree(root: BagInfo): Node = {
    val children = bagMap(root.copy(amount = 1))
    Node(root, children.map(c => buildTree(c.copy(amount = c.amount * root.amount))))
  }

  println(buildTree(BagInfo(1, "shiny gold")).amount - 1)
}
