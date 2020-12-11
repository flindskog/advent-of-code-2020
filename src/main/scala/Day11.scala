import scala.annotation.tailrec
import scala.io.Source

trait Day11 {
  val data: LazyList[String] =
    Source
      .fromInputStream(getClass.getResourceAsStream("input_11.txt"))
      .getLines()
      .map(_.trim)
      .to(LazyList)

  val Floor    = '.'
  val Empty    = 'L'
  val Occupied = '#'

  val grid: Array[Array[Char]] = data.map(_.toArray).toArray
  val gridWidth: Int           = grid(0).length
  val gridLength: Int          = grid.length

  type FindSeatsFn = (Int, Int, Array[Array[Char]]) => Seq[Char]

  def adjacent(x: Int, y: Int, grid: Array[Array[Char]]): Seq[Char] =
    for {
      xs <- x - 1 to x + 1
      ys <- y - 1 to y + 1
      if !outOfBounds(xs, ys) && ((xs, ys) != (x, y))
    } yield grid(ys)(xs)

  val allDirections: Seq[(Int, Int)] = for {
    deltaX <- -1 to 1
    deltaY <- -1 to 1
    if (deltaX, deltaY) != (0, 0)
  } yield (deltaX, deltaY)

  def outOfBounds(x: Int, y: Int): Boolean = x < 0 || x >= gridWidth || y < 0 || y >= gridLength

  def inSight(x: Int, y: Int, grid: Array[Array[Char]]): Seq[Char] = {
    @tailrec
    def findInDirection(x: Int, y: Int, direction: (Int, Int)): Option[Char] =
      direction match {
        case (dX, dY) =>
          val (cx, cy) = (x + dX, y + dY)
          if (outOfBounds(cx, cy)) None
          else if (grid(cy)(cx) != Floor) Some(grid(cy)(cx))
          else findInDirection(cx, cy, direction)
      }

    allDirections.flatMap(d => findInDirection(x, y, d))
  }

  @tailrec
  final def transform(grid: Array[Array[Char]], findFn: FindSeatsFn, least: Int): Array[Array[Char]] = {
    val next = grid.map(_.zipWithIndex).zipWithIndex.map {
      case (row: Array[(Char, Int)], y: Int) =>
        row.map {
          case (c, x) =>
            c match {
              case Empty if !findFn(x, y, grid).contains(Occupied)              => Occupied
              case Occupied if findFn(x, y, grid).count(_ == Occupied) >= least => Empty
              case c                                                            => c
            }
        }
    }

    if (next.zip(grid).forall { case (n, g) => n sameElements g }) next
    else transform(next, findFn, least)
  }

  def transformAdjacent(grid: Array[Array[Char]]): Array[Array[Char]] = transform(grid, adjacent, 4)

  def transformInSight(grid: Array[Array[Char]]): Array[Array[Char]] = transform(grid, inSight, 5)
}

object Day11_1 extends App with Day11 {
  println(transformAdjacent(grid).map(_.count(_ == Occupied)).sum)
}

object Day11_2 extends App with Day11 {
  println(transformInSight(grid).map(_.count(_ == Occupied)).sum)
}
