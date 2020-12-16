import scala.annotation.tailrec
import scala.io.Source
import scala.util.matching.Regex

trait Day16 {
  val data: LazyList[String] =
    Source
      .fromInputStream(getClass.getResourceAsStream("input_16.txt"))
      .getLines()
      .map(_.trim)
      .to(LazyList)

  val groups: List[List[String]] = data
    .foldLeft(List(List.empty[String])) { (groups, line) =>
      if (line.isEmpty)
        Nil :: groups
      else
        (groups.head :+ line) :: groups.tail
    }
    .reverse

  case class Range(start: Long, end: Long) {
    def matches(v: Long): Boolean = v >= start && v <= end
  }

  object Range {
    def apply(str: String): Range = {
      val s = str.split("-", 2)
      new Range(s(0).toLong, s(1).toLong)
    }
  }

  val ruleRegex: Regex = """^(.*): (\d+-\d+) or (\d+-\d+)$""".r
  val rules: List[(String, List[Range])] = groups(0).map {
    case ruleRegex(name, r1, r2) => (name, List(Range(r1), Range(r2)))
  }
  val myTicket: List[Long]            = groups(1).drop(1).head.split(",").map(_.toLong).toList
  val nearbyTickets: List[List[Long]] = groups(2).drop(1).map(_.split(",").map(_.toLong).toList)

  def notValidValues(ticket: List[Long]): List[Long] =
    ticket.filter(v =>
      rules.forall {
        case (_, ranges) => !ranges.exists(_.matches(v))
      }
    )

}

object Day16_1 extends App with Day16 {
  println(nearbyTickets.map(notValidValues).filter(_.nonEmpty).map(_.sum).sum)

}

object Day16_2 extends App with Day16 {
  val validNearbyTickets = nearbyTickets.filter(notValidValues(_).isEmpty)
  val byCol              = validNearbyTickets.transpose.zipWithIndex
  val suspects: List[List[(String, List[Range])]] = byCol.map {
    case (values, _) =>
      rules.filter {
        case (_, ranges) => values.forall(v => ranges.exists(_.matches(v)))
      }
  }
  @tailrec
  def reduce(res: List[List[(String, List[Range])]]): List[(String, List[Day16_2.Range])] = {
    val (determined, ambiguous) = res.partition(_.size == 1)
    val determinedRules         = determined.flatten.toSet
    if (ambiguous.isEmpty) determined.map(_.head)
    else reduce(res.map(r => if (r.size == 1) r else r.filterNot(determinedRules.contains)))
  }
  val r = reduce(suspects)
    .zip(myTicket)
    .filter { case ((name, _), _) => name.startsWith("departure") }
    .map { case ((_, _), value) => value }
    .product

  println(r)
}
