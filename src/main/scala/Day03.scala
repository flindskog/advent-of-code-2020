import scala.annotation.tailrec
import scala.io.Source

trait Day03 {
  lazy val data: LazyList[String] =
    Source
      .fromInputStream(getClass.getResourceAsStream("input_03.txt"))
      .getLines()
      .map(_.trim)
      .to(LazyList)

  val map: List[List[Char]] = data.toList.map(_.toList)

  val patternWidth: Int = map.head.size

  case class Coordinate(x: Int, y: Int) {
    def +(c: Coordinate): Coordinate = Coordinate(x + c.x, y + c.y)
  }

  val origo: Coordinate = Coordinate(0, 0)

  def isLegalCoordinate(c: Coordinate): Boolean = c.y < map.size

  def terrainAt(coordinate: Coordinate): Char =
    map(coordinate.y)(coordinate.x % patternWidth)

  def countTrees(movement: Coordinate): Long = {
    @tailrec
    def move0(
        move: Coordinate,
        position: Coordinate = origo,
        accTerrain: List[Char] = Nil
    ): List[Char] = {
      val terrains = terrainAt(position) :: accTerrain
      val next     = position + move
      if (isLegalCoordinate(next)) move0(move, next, terrains)
      else terrains
    }

    val path = move0(movement)
    path.count(_ == '#')
  }
}

object Day03_1 extends App with Day03 {
  println(
    countTrees(Coordinate(3, 1))
  )
}

object Day03_2 extends App with Day03 {
  println(
    List(
      Coordinate(1, 1),
      Coordinate(3, 1),
      Coordinate(5, 1),
      Coordinate(7, 1),
      Coordinate(1, 2)
    ).map(countTrees).product
  )
}
