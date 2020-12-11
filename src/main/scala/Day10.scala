import scala.annotation.tailrec
import scala.collection.immutable.{SortedMap, TreeMap}
import scala.io.Source

trait Day10 {
  lazy val data: LazyList[Long] =
    Source
      .fromInputStream(getClass.getResourceAsStream("input_10.txt"))
      .getLines()
      .map(_.trim.toLong)
      .to(LazyList)
      .sorted

  val full: Seq[Long]  = 0L +: data :+ data.last + 3
  val diffs: Seq[Long] = full.tail.zip(full).map { case (h, l) => h - l }
}

object Day10_1 extends App with Day10 {
  println(diffs.count(_ == 1) * diffs.count(_ == 3))
}

object Day10_2 extends App with Day10 {
  @tailrec
  def combs(l: List[Long], acc: SortedMap[Long, Long]): Long =
    l match {
      case Nil =>
        acc.max._2
      case x :: xs =>
        val c = acc(x - 3) + acc(x - 2) + acc(x - 1)
        combs(xs, acc + (x -> c))
    }

  println(combs(full.tail.toList, TreeMap[Long, Long](0L -> 1L).withDefaultValue(0)))
}
