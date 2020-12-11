import scala.annotation.tailrec
import scala.io.Source

trait Day09 {
  lazy val data: Seq[Long] =
    Source
      .fromInputStream(getClass.getResourceAsStream("input_09.txt"))
      .getLines()
      .map(_.trim.toLong)
      .to(LazyList)

  val cipherLength = 25
  val illegal: Seq[(Long, Seq[Long])] = data.drop(cipherLength).zip(data.sliding(cipherLength)).filterNot {
    case (num, prev) => prev.combinations(2).map(_.sum).contains(num)
  }
}

object Day09_1 extends App with Day09 {
  println(illegal.head._1)
}

object Day09_2 extends App with Day09 {

  def findSum(n: Long, prev: List[Long]): List[List[Long]] = {
    @tailrec
    def find0(n: Long, prev: List[Long], acc: List[Long]): List[Long] =
      if (acc.sum == n) acc
      else if (acc.sum > n) Nil
      else find0(n, prev.tail, prev.head :: acc)

    find0(n, prev, Nil) :: (prev.tail match {
      case Nil => Nil
      case xs  => findSum(n, xs)
    })
  }

  val numbers  = findSum(illegal.head._1, data.toList).filterNot(_.isEmpty).head
  val solution = numbers.min + numbers.max

  println(solution)
}
