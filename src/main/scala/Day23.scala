import scala.annotation.tailrec
import scala.collection.immutable.List

trait Day23 {
  case class CircularList[A](value: A, var next: CircularList[A], var prev: CircularList[A]) {
    private def addValue(v: A): CircularList[A] = {
      val l = CircularList(v, next, this)
      next = l
      l.next.prev = l
      l
    }
    def head: A = value

    def removeList(n: Int): CircularList[A] = {
      val newHead     = skip(n)
      val removedTail = newHead.prev
      prev.next = newHead
      newHead.prev = prev
      prev = removedTail
      removedTail.next = this
      this
    }

    def insertList(l: CircularList[A]): CircularList[A] = {
      next.prev = l.prev
      l.prev.next = next
      next = l
      l.prev = this
      l
    }

    @tailrec
    private def skip(n: Int): CircularList[A] =
      if (n == 0) this
      else next.skip(n - 1)

    @tailrec
    final def toList(i: Int, acc: List[A] = Nil): List[A] =
      if (i == 0) acc
      else next.toList(i - 1, value :: acc)
  }

  object CircularList {
    def apply[T](v: T): CircularList[T] = {
      val l = new CircularList[T](v, null, null)
      l.next = l
      l.prev = l
      l
    }
    def apply[T](list: List[T]): CircularList[T] = {
      val (head, tail) = (list.head, list.tail)
      val l            = CircularList(head)
      tail.foldLeft(l) { case (acc, v) => acc.addValue(v) }.next
    }
  }

  val data: List[Int] = "952316487".toCharArray.map(_.asDigit).toList

  @tailrec
  final def buildCache(
      l: CircularList[Int],
      size: Int,
      acc: Map[Int, CircularList[Int]] = Map()
  ): Map[Int, CircularList[Int]] =
    if (size == 0) acc
    else buildCache(l.next, size - 1, acc.updated(l.head, l))

  @tailrec
  final def move(
      maxValue: Int,
      deck: CircularList[Int],
      cache: Map[Int, CircularList[Int]],
      turns: Int,
      turn: Int = 1
  ): CircularList[Int] = {
    val current      = deck.head
    val picked       = deck.next.removeList(3)
    val pickedValues = picked.toList(3).toSet

    @tailrec
    def dest(d: Int): Int =
      if (d < 1) dest(maxValue)
      else if (pickedValues.contains(d)) dest(d - 1)
      else d

    val destination = dest(current - 1)
    val insertPoint = cache(destination)
    insertPoint.insertList(picked)

    if (turn == turns) deck.next
    else move(maxValue, deck.next, cache, turns, turn + 1)
  }
}

object Day23_1 extends App with Day23 {
  val maxValue = 9
  val deck     = CircularList(data)
  val cache    = buildCache(deck, maxValue)
  val res      = move(maxValue, deck, cache, 100)
  println(cache(1).next.toList(8).mkString)
}

object Day23_2 extends App with Day23 {
  val maxValue = 1000000
  val deck     = CircularList((data ++: (data.size + 1 to maxValue)).toList)
  val cache    = buildCache(deck, maxValue)
  val res      = move(maxValue, deck, cache, 10000000)
  println(cache(1).next.toList(2).map(_.toLong).product)
}
