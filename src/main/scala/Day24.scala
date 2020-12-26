import scala.annotation.tailrec
import scala.io.Source

trait Day24 {
  implicit class IntOps(i: Int) {
    def between(from: Int, to: Int): Boolean = i >= from && i <= to
  }

  val data: List[String] =
    Source
      .fromInputStream(getClass.getResourceAsStream("input_24.txt"))
      .getLines()
      .map(_.trim)
      .toList

  case class HexPos(q: Int, r: Int) {
    def +(that: HexPos): HexPos = HexPos(q + that.q, r + that.r)
  }

  val southEast: HexPos = HexPos(0, 1)
  val southWest: HexPos = HexPos(-1, 1)
  val northEast: HexPos = HexPos(1, -1)
  val northWest: HexPos = HexPos(0, -1)
  val east: HexPos      = HexPos(1, 0)
  val west: HexPos      = HexPos(-1, 0)

  val allDirections = Set(southEast, southWest, northEast, northWest, east, west)

  def toDirections(l: List[Char]): List[HexPos] =
    l match {
      case 's' :: 'e' :: xs => southEast :: toDirections(xs)
      case 's' :: 'w' :: xs => southWest :: toDirections(xs)
      case 'n' :: 'e' :: xs => northEast :: toDirections(xs)
      case 'n' :: 'w' :: xs => northWest :: toDirections(xs)
      case 'e' :: xs        => east :: toDirections(xs)
      case 'w' :: xs        => west :: toDirections(xs)
      case Nil              => Nil
    }

  val directions: List[List[HexPos]] = data.map(line => toDirections(line.toCharArray.toList))
  val blacks: Set[HexPos] = directions
    .map(_.reduce(_ + _))
    .foldLeft(Set[HexPos]())((acc, tile) => if (acc.contains(tile)) acc - tile else acc + tile)

  def adjacent(pos: HexPos): Set[HexPos] = allDirections.map(_ + pos)

  @tailrec
  final def cycle(blacks: Set[HexPos], times: Int): Set[HexPos] = {
    def blackAdjacents(pos: HexPos): Int = (adjacent(pos) intersect blacks).size

    val whites       = blacks.flatMap(adjacent) -- blacks
    val stayBlack    = blacks.filter(p => blackAdjacents(p).between(1, 2))
    val whiteToBlack = whites.filter(p => blackAdjacents(p) == 2)
    val res          = stayBlack ++ whiteToBlack
    if (times > 1) cycle(res, times - 1)
    else res
  }

}

object Day24_1 extends App with Day24 {
  println(blacks.size)
}

object Day24_2 extends App with Day24 {
  println(cycle(blacks, 100).size)
}
