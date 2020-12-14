import scala.annotation.tailrec
import scala.io.Source
import scala.math.abs
import scala.util.matching.Regex

trait Day12 {
  val data: LazyList[String] =
    Source
      .fromInputStream(getClass.getResourceAsStream("input_12.txt"))
      .getLines()
      .map(_.trim)
      .to(LazyList)

  case class Coordinate(lat: Int, long: Int) {
    def +(that: Coordinate): Coordinate = Coordinate(lat + that.lat, long + that.long)
    def *(length: Int): Coordinate      = Coordinate(lat * length, long * length)
    @tailrec
    final def rotateClockwise(degrees: Int): Coordinate =
      if (degrees == 0) this else Coordinate(-long, lat).rotateClockwise(degrees - 90)

    def manhattanDistanceTo(that: Coordinate): Int = abs(lat - that.lat) + abs(long - that.long)
  }

  object Coordinate {
    val origo: Coordinate          = Coordinate(0, 0)
    val (north, east, south, west) = (Coordinate(1, 0), Coordinate(0, 1), Coordinate(-1, 0), Coordinate(0, -1))
  }

  type Direction = Coordinate

  sealed trait Instruction
  case class Go(direction: Direction, length: Int) extends Instruction
  case class RotateClockwise(angle: Int)           extends Instruction
  case class Forward(length: Int)                  extends Instruction

  val instruction: Regex = """^([NEWSLRF])(\w+)$""".r

  val instructions: Seq[Instruction] = data.map {
    case instruction("N", l) => Go(Coordinate.north, l.toInt)
    case instruction("E", l) => Go(Coordinate.east, l.toInt)
    case instruction("W", l) => Go(Coordinate.west, l.toInt)
    case instruction("S", l) => Go(Coordinate.south, l.toInt)
    case instruction("R", a) => RotateClockwise(a.toInt)
    case instruction("L", a) => RotateClockwise(360 - a.toInt)
    case instruction("F", l) => Forward(l.toInt)
  }

  case class State(coordinate: Coordinate, direction: Direction)
}

object Day12_1 extends App with Day12 {
  val mapping = instructions.map {
    case Go(direction, length) =>
      (state: State) => state.copy(coordinate = state.coordinate + direction * length)

    case RotateClockwise(angle) =>
      (state: State) => state.copy(direction = state.direction.rotateClockwise(angle))

    case Forward(length) =>
      (state: State) => state.copy(coordinate = state.coordinate + state.direction * length)
  }

  val end = mapping.foldLeft(State(Coordinate.origo, Coordinate(0, 1)))((acc, n) => n(acc))
  println(Coordinate.origo.manhattanDistanceTo(end.coordinate))
}

object Day12_2 extends App with Day12 {
  val mapping = instructions.map {
    case Go(direction, length) =>
      (state: State) => state.copy(direction = state.direction + direction * length)

    case RotateClockwise(angle) =>
      (state: State) => state.copy(direction = state.direction.rotateClockwise(angle))

    case Forward(length) =>
      (state: State) => state.copy(coordinate = state.coordinate + state.direction * length)
  }

  val end = mapping.foldLeft(State(Coordinate.origo, Coordinate(1, 10)))((acc, n) => n(acc))
  println(Coordinate.origo.manhattanDistanceTo(end.coordinate))
}
