import scala.annotation.tailrec
import scala.io.Source
import scala.util.matching.Regex

trait Day20 {
  val data: List[String] =
    Source
      .fromInputStream(getClass.getResourceAsStream("input_20.txt"))
      .getLines()
      .map(_.trim)
      .toList

  case class Tile(id: Long, data: List[List[Char]]) {
    val topEdge: List[Char]     = data.head
    val rightEdge: List[Char]   = data.map(d => d(data.head.size - 1))
    val bottomEdge: List[Char]  = data.last.reverse
    val leftEdge: List[Char]    = data.map(_.head).reverse
    val edges: List[List[Char]] = List(topEdge, rightEdge, bottomEdge, leftEdge)
    lazy val orientations: List[Tile] = List(
      this,
      rotatedRight,
      rotatedRight.rotatedRight,
      rotatedRight.rotatedRight.rotatedRight,
      flipped,
      flipped.rotatedRight,
      flipped.rotatedRight.rotatedRight,
      flipped.rotatedRight.rotatedRight.rotatedRight
    )

    def flipped: Tile      = Tile(id, data.transpose)
    def rotatedRight: Tile = Tile(id, flipped.data.map(_.reverse))

    def matches(that: Tile): Boolean        = ((edges ++ flipped.edges) intersect (that.edges ++ that.flipped.edges)).nonEmpty
    def matchesEdge(e: List[Char]): Boolean = (edges ++ flipped.edges) contains e
    def stripEdges: Tile                    = Tile(id, data.map(_.tail.init).tail.init)
    override def toString: String           = data.map(_.mkString).mkString("\n")
  }

  def printTiles(l: List[Tile]): String =
    l.map(_.data.map(_.mkString)).transpose.map(_.mkString("")).mkString("\n")

  val tileRegex: Regex = """^Tile (\w+):$""".r

  def toTile(l: List[String]): Tile =
    (l.head, l.tail) match {
      case (tileRegex(id), data) => Tile(id.toLong, data.map(_.toCharArray.toList))
    }

  val tiles: List[Tile] = data
    .foldLeft(List(List.empty[String])) { (groups, line) =>
      if (line.isEmpty)
        Nil :: groups
      else
        (groups.head :+ line) :: groups.tail
    }
    .reverse
    .map(toTile)

  val tileMap: Map[Long, Tile] = tiles.map(t => (t.id, t)).toMap

  val sideLength: Int = math.sqrt(tiles.size).toInt

  val matching: Map[Long, Set[Long]] =
    (for {
      t1 <- tiles
    } yield (t1.id, (tiles.toSet - t1).filter(_.matches(t1)).map(_.id))).toMap

  val corners: List[Long] = matching.filter {
    case (_, matches) => matches.size == 2
  }.map {
    case (id, _) => id
  }.toList

  assert(corners.size == 4)

  @tailrec
  final def buildDown(tile: Tile, acc: List[Tile]): List[Tile] = {
    val candidates = matching(tile.id).toList.map(tileMap)
    val res        = candidates.flatMap(_.orientations).filter(c => c.topEdge == tile.bottomEdge.reverse)
    res match {
      case value :: Nil => buildDown(value, acc :+ value)
      case Nil          => acc
    }
  }
  @tailrec
  final def buildRight(tile: Tile, acc: List[Tile]): List[Tile] = {
    val candidates = matching(tile.id).toList.map(tileMap)
    val res        = candidates.flatMap(_.orientations).filter(c => c.leftEdge == tile.rightEdge.reverse)
    res match {
      case value :: Nil => buildRight(value, acc :+ value)
      case Nil          => acc
    }
  }
}

object Day20_1 extends App with Day20 {
  println(corners.product)
}

object Day20_2 extends App with Day20 {
  // We can pick any corner to start with, doesn't matter
  val topLeft   = tileMap(corners.head)
  val matches   = matching(topLeft.id).toList.map(tileMap)
  val nextRight = matches.head
  val nextDown  = matches(1)

  // Right and bottom edge should match
  val start =
    topLeft.orientations.find(t => nextRight.matchesEdge(t.rightEdge) && nextDown.matchesEdge(t.bottomEdge)).get

  val firstRow = buildDown(start, List(start))
  val all      = firstRow.map(t => buildRight(t, List(t)))
  val noEdges  = all.map(_.map(_.stripEdges))

  val noEdgesTile =
    Tile(0, noEdges.flatMap(_.map(_.data.map(_.mkString)).transpose.map(_.mkString("").toCharArray.toList)))

  val monster =
    """                  # 
    |#    ##    ##    ###
    | #  #  #  #  #  #   """.stripMargin.split("\n")

  val monsterCoords = monster.zipWithIndex.flatMap {
    case (line, y) => line.zipWithIndex.filter(_._1 == '#').map(i => (i._2, y))
  }.toSet

  val monsters = noEdgesTile.orientations.map { t =>
    val a           = t.data.map(_.toArray).toArray
    val monsterSize = (monsterCoords.map(_._1).max, monsterCoords.map(_._2).max)

    def checkForMonster(x: Int, y: Int): Boolean =
      monsterCoords.forall { case (x1, y1) => a(y + y1)(x + x1) == '#' }

    (for {
      x <- 0 until a.head.length - monsterSize._1
      y <- 0 until a.length - monsterSize._2
    } yield checkForMonster(x, y)).count(_ == true)
  }.max

  println(noEdgesTile.data.map(_.count(_ == '#')).sum - monsters * monsterCoords.size)
}
