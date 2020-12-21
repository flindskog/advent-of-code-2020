import scala.annotation.tailrec
import scala.io.Source
import scala.util.matching.Regex

trait Day21 {
  val data: List[String] =
    Source
      .fromInputStream(getClass.getResourceAsStream("input_21.txt"))
      .getLines()
      .map(_.trim)
      .toList

  val lineRegex: Regex = """(.*) \(contains (.*)\)""".r

  val dataSets: List[(Set[String], Set[String])] = data.map {
    case lineRegex(ingredients, allergenes) => (ingredients.split(" ").toSet, allergenes.split(", ").toSet)
  }

  val perAllergen: Map[String, Set[String]] = dataSets.flatMap {
    case (ingredients, allergenes) => allergenes.map(a => (a, ingredients))
  }.groupBy(_._1).view.mapValues(_.map(_._2).reduce(_ intersect _)).toMap

  @tailrec
  final def reduce(m: Map[String, Set[String]]): Map[String, Set[String]] = {
    val (found, notFound) = m.partition(_._2.size == 1)
    val foundAllergens    = found.values.flatten.toSet
    val filtered = notFound.map {
      case (ingredients, allergenes) => (ingredients, allergenes diff foundAllergens)
    }
    if (filtered.forall(_._2.size <= 1)) found ++ filtered
    else reduce(found ++ filtered)
  }

  val foundAllergenes: Map[String, String] = reduce(perAllergen).filter(_._2.size == 1).view.mapValues(_.head).toMap
  val noAllergenes: List[Set[String]]      = dataSets.map(_._1.diff(foundAllergenes.values.toSet))
}

object Day21_1 extends App with Day21 {
  println(noAllergenes.map(_.size).sum)
}

object Day21_2 extends App with Day21 {
  println(foundAllergenes.toList.sortBy(_._1).map(_._2).mkString(","))
}
