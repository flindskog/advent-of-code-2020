import scala.io.Source

trait Day06 {
  lazy val data: LazyList[String] =
    Source
      .fromInputStream(getClass.getResourceAsStream("input_06.txt"))
      .getLines()
      .map(_.trim)
      .to(LazyList)

  val groups: List[List[List[Char]]] = data
    .foldLeft(List(List.empty[String])) { (groups, line) =>
      if (line.isEmpty) Nil :: groups
      else (line :: groups.head) :: groups.tail
    }
    .map(_.map(_.toList))
}

object Day06_1 extends App with Day06 {
  println(groups.map(_.reduce(_ ::: _).distinct.size).sum)
}

object Day06_2 extends App with Day06 {
  println(groups.map(_.reduce(_ intersect _).size).sum)
}
