import scala.io.Source
import scala.util.matching.Regex

trait Day02 {
  lazy val data: LazyList[String] =
    Source
      .fromInputStream(getClass.getResourceAsStream("input_02.txt"))
      .getLines()
      .map(_.trim)
      .to(LazyList)

  val format: Regex = """(\d+)-(\d+) (\w): (\w+)""".r
}

object Day02_1 extends App with Day02 {
  val valid = data.count {
    case format(min, max, letter, password) =>
      s"^([^$letter]*$letter){$min,$max}[^$letter]*$$".r.matches(password)
  }

  println(valid)
}

object Day02_2 extends App with Day02 {
  val valid = data.count {
    case format(pos1, pos2, letter, password) =>
      val d1 = pos1.toInt - 1
      val d2 = pos2.toInt - 1
      val v1 = s"^.{$d1}$letter.*$$".r.matches(password)
      val v2 = s"^.{$d2}$letter.*$$".r.matches(password)
      v1 ^ v2
  }

  println(valid)
}
