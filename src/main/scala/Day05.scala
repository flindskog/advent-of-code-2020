import scala.annotation.tailrec
import scala.io.Source
import scala.util.matching.Regex

trait Day05 {
  lazy val data: LazyList[String] =
    Source
      .fromInputStream(getClass.getResourceAsStream("input_05.txt"))
      .getLines()
      .map(_.trim)
      .to(LazyList)

  val (rows, cols)      = (128, 8)
  val posPattern: Regex = s"([F|B]{7})([L|R]{3})".r

  @tailrec
  final def pos(patttern: List[Char], low: Int, high: Int): Int = {
    val half = low + ((high - low) + 1) / 2
    patttern match {
      case ('F' | 'L') :: xs => pos(xs, low, half - 1)
      case ('B' | 'R') :: xs => pos(xs, half, high)
      case _                 => low
    }
  }

  def position: String => (Int, Int) = {
    case posPattern(rowP, colP) => (pos(rowP.toList, 0, rows - 1), pos(colP.toList, 0, cols - 1))
  }

  def rowId: ((Int, Int)) => Int = { case (row: Int, col: Int) => row * cols + col }

  def toRowId: String => Int = position andThen rowId
}

object Day05_1 extends App with Day05 {
  println(data.map(toRowId).max)
}

object Day05_2 extends App with Day05 {
  @tailrec
  final def firstMissing(l: List[Int]): Int =
    l match {
      case x :: y :: xs => if (y - x != 1) x + 1 else firstMissing(y :: xs)
      case _            => -1
    }

  println(firstMissing(data.map(toRowId).sorted.toList))
}
