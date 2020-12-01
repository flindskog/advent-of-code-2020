import scala.io.Source

trait Day01 {
  lazy val data: LazyList[(Int, Int)] =
    Source
      .fromInputStream(getClass.getResourceAsStream("input_01.txt"))
      .getLines()
      .map(_.trim)
      .map(_.toInt)
      .to(LazyList)
      .zipWithIndex
}

object Day01_1 extends App with Day01 {
  val solutions = for {
    (val1, idx1) <- data
    (val2, idx2) <- data if idx1 < idx2 && val1 + val2 == 2020
  } yield val1 * val2

  assert(solutions.size == 1)
  println(solutions.toList.head)
}

object Day01_2 extends App with Day01 {
  val solutions = for {
    (val1, idx1) <- data
    (val2, idx2) <- data if idx1 < idx2
    (val3, idx3) <- data if idx2 < idx3 && val1 + val2 + val3 == 2020
  } yield val1 * val2 * val3

  assert(solutions.size == 1)
  println(solutions.toList.head)
}
