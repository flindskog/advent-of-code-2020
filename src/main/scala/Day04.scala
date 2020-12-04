import scala.io.Source

trait Day04 {
  lazy val data: LazyList[String] =
    Source
      .fromInputStream(getClass.getResourceAsStream("input_04.txt"))
      .getLines()
      .map(_.trim)
      .to(LazyList)

  val onePerLine: List[String] = data
    .foldLeft(List(List.empty[String])) { (groups, line) =>
      if (line.isEmpty) Nil :: groups
      else (line :: groups.head) :: groups.tail
    }
    .map(_.mkString(" "))

  val fieldMap: List[Map[String, String]] = onePerLine.map { line =>
    val splitted = line.split(" ").map(_.split(":", 2))
    splitted.collect {
      case Array(key: String, value: String) => (key, value)
    }.toMap
  }

  val required = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

  def isValid(input: Map[String, String]): Boolean = required.subsetOf(input.keySet)
}

object Day04_1 extends App with Day04 {
  println(fieldMap.count(isValid))
}

object Day04_2 extends App with Day04 {
  def isNumber(v: String, noDigits: Int, min: Int, max: Int): Boolean =
    v.length == noDigits && v.toInt >= min && v.toInt <= max

  def isValidHeight(c: String): Boolean = {
    val r = """^(\d+)(cm|in)$""".r
    c match {
      case r(length, unit) =>
        val l = length.toInt
        if (unit == "cm") l >= 150 && l <= 193
        else if (unit == "in") l >= 59 && l <= 76
        else false
      case _ => false
    }
  }

  val aHairColor = "^#[0-9|a-f]{6}$".r
  val anEyeColor = "^amb|blu|brn|gry|grn|hzl|oth$".r

  val withValidValues = fieldMap.map(_.filter { e =>
    e match {
      case ("byr", v)            => isNumber(v, 4, 1920, 2002)
      case ("iyr", v)            => isNumber(v, 4, 2010, 2020)
      case ("eyr", v)            => isNumber(v, 4, 2020, 2030)
      case ("hgt", v)            => isValidHeight(v)
      case ("hcl", aHairColor()) => true
      case ("ecl", anEyeColor()) => true
      case ("pid", v)            => isNumber(v, 9, 0, Int.MaxValue)
      case ("cid", _)            => true
      case _                     => false
    }
  })

  println(withValidValues.count(isValid))
}
