import scala.annotation.tailrec
import scala.io.Source
import scala.util.matching.Regex

trait Day08 {
  val regex: Regex = """^(acc|jmp|nop) ([+-]\d+)$""".r
  lazy val data: Array[(String, Int)] =
    Source
      .fromInputStream(getClass.getResourceAsStream("input_08.txt"))
      .getLines()
      .map(_.trim)
      .to(LazyList)
      .map {
        case regex(i, v) => (i, v.toInt)
      }
      .toArray

  case class State(
      acc: Int,
      current: Int,
      instructions: Array[(String, Int)],
      visited: Set[Int],
      patched: Boolean = false
  ) {
    def next(acc: Int, jmp: Int): State = State(this.acc + acc, current + jmp, instructions, visited + current, patched)
    def patch: State                    = this.copy(patched = true)
    def completed: Boolean              = current == instructions.length
    def infiniteLoop: Boolean           = visited.contains(current)
  }
}

object Day08_1 extends App with Day08 {
  @tailrec
  def calculate(state: State): State =
    if (state.infiniteLoop || state.completed) state
    else
      state.instructions(state.current) match {
        case ("acc", v) => calculate(state.next(v, 1))
        case ("jmp", v) => calculate(state.next(0, v))
        case ("nop", _) => calculate(state.next(0, 1))
      }
  println(calculate(State(0, 0, data, Set())).acc)
}

object Day08_2 extends App with Day08 {
  def calculate(state: State): List[State] =
    if (state.infiniteLoop || state.completed) List(state)
    else
      state.instructions(state.current) match {
        case ("acc", v) => calculate(state.next(v, 1))
        case ("jmp", v) =>
          calculate(state.next(0, v)) ::: (if (state.patched) Nil else calculate(state.patch.next(0, 1)))
        case ("nop", v) => calculate(state.next(0, 1)) ::: (if (state.patched) Nil else calculate(state.next(0, v)))

      }
  println(
    calculate(State(0, 0, data, Set())).filter(_.completed).map(_.acc).head
  )
}
