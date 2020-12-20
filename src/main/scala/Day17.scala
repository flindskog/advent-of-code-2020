import scala.annotation.tailrec
import scala.io.Source

trait Day17 {
  val data: LazyList[String] =
    Source
      .fromInputStream(getClass.getResourceAsStream("input_17.txt"))
      .getLines()
      .map(_.trim)
      .to(LazyList)

  implicit class IntOps(i: Int) {
    def between(from: Int, to: Int): Boolean = i >= from && i <= to
  }

  case class Pos(coord: List[Int])

  def adjacent(pos: Pos): Set[Pos] = {
    def combs[A](xss: List[List[A]]): List[List[A]] =
      xss match {
        case Nil => List(Nil)
        case xs :: rss =>
          for {
            x  <- xs
            cs <- combs(rss)
          } yield x :: cs
      }
    combs(pos.coord.map(p => (p - 1 to p + 1).toList)).map(Pos).toSet.filterNot(_ == pos)
  }

  def active(dimensions: Int): Set[Pos] =
    data.zipWithIndex.flatMap {
      case (ys, x) =>
        ys.toCharArray.zipWithIndex.flatMap {
          case (s, y) =>
            s match {
              case '#' => Some(Pos(List(x, y) ::: List.fill(dimensions - 2)(0)))
              case _   => None
            }
        }
    }.toSet

  @tailrec
  final def cycle(active: Set[Pos], times: Int): Set[Pos] = {
    def activeAdjacent(pos: Pos): Int = (adjacent(pos) intersect active).size

    val inactive = active.flatMap(adjacent) -- active
    val a1       = active.filter(p => activeAdjacent(p).between(2, 3))
    val a2       = inactive.filter(p => activeAdjacent(p) == 3)
    val res      = a1 ++ a2
    if (times > 1) cycle(res, times - 1)
    else res
  }
}

object Day17_1 extends App with Day17 {
  println(cycle(active(dimensions = 3), 6).size)
}

object Day17_2 extends App with Day17 {
  println(cycle(active(dimensions = 4), 6).size)
}
