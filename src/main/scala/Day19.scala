import scala.io.Source
import scala.util.matching.Regex

trait Day19 {
  val data: List[String] =
    Source
      .fromInputStream(getClass.getResourceAsStream("input_19.txt"))
      .getLines()
      .map(_.trim)
      .toList

  val groups: List[List[String]] = data
    .foldLeft(List(List.empty[String])) { (groups, line) =>
      if (line.isEmpty)
        Nil :: groups
      else
        (groups.head :+ line) :: groups.tail
    }
    .reverse

  val ruleRegex: Regex  = """^(\d+): (.*)$""".r
  val matchRegex: Regex = """^"(\w)"$""".r

  sealed trait Rule
  case class Match(c: Char)        extends Rule
  case class Refs(ids: List[Int])  extends Rule
  case class Or(rules: List[Rule]) extends Rule

  def parseRule(str: String): (Int, Rule) =
    str match {
      case ruleRegex(id, ruleString) =>
        val rule = ruleString match {
          case matchRegex(c) => Match(c.head)
          case r             => Or(r.split(""" \| """).map(s => Refs(s.split(" ").map(_.toInt).toList)).toList)
        }
        (id.toInt, rule)
    }

  val rules: Map[Int, Rule]  = groups.head.map(parseRule).toMap
  val messages: List[String] = groups.tail.head
  val longestMessage: Int    = messages.map(_.length).max

  def unwind(r: Rule, allRules: Map[Int, Rule], counter: Int = 0): String =
    if (counter > longestMessage) "" // Stop the infinite recursion on the longest message length
    else
      r match {
        case Match(c)  => c.toString
        case Refs(ids) => ids.foldLeft("") { case (acc, id) => acc + unwind(allRules(id), allRules, counter + 1) }
        case Or(rules) => rules.map(r => unwind(r, allRules, counter + 1)).mkString("(:?", "|", ")")
      }

}

object Day19_1 extends App with Day19 {
  val matchingRegex = unwind(rules(0), rules).r
  println(messages.count(matchingRegex.matches))
}

object Day19_2 extends App with Day19 {
  val updated =
    """8: 42 | 42 8
    |11: 42 31 | 42 11 31""".stripMargin.split("\n").map(parseRule)

  val newRules = updated.foldLeft(rules) { (m, r) =>
    m.updated(r._1, r._2)
  }

  val matchingRegex = unwind(newRules(0), newRules).r
  println(messages.count(matchingRegex.matches))
}
