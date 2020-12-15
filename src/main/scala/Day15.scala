trait Day15 {
  val data = List(1, 20, 8, 12, 0, 14)

  case class State(last: Int, turn: Int, prevSpoken: Map[Int, Int]) {
    def next: State = {
      val n = prevSpoken.get(last).map(turn - _).getOrElse(0)
      State(n, turn + 1, prevSpoken.updated(last, turn))
    }
  }

  object State {
    def init(numbers: List[Int]): State =
      State(numbers.last, numbers.size - 1, numbers.init.zipWithIndex.toMap)
  }

  def nth(n: Int): State = (0 to n - 1 - data.size).foldLeft(State.init(data))((s, _) => s.next)
}

object Day15_1 extends App with Day15 {
  println(nth(2020).last)
}

object Day15_2 extends App with Day15 {
  println(nth(30000000).last)
}
