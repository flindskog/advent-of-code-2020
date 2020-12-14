import java.time.OffsetDateTime

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

trait Day13 {
  val data: List[String] =
    Source
      .fromInputStream(getClass.getResourceAsStream("input_13.txt"))
      .getLines()
      .map(_.trim)
      .toList

  case class Bus(id: Int, next: LazyList[Long])

  object Bus {
    def apply(id: Int, startAt: Long = 0): Bus = {
      def departures(start: Long): LazyList[Long] = start #:: departures(start + id)
      new Bus(id, departures(0))
    }
  }

  implicit object BusOrdering extends Ordering[Bus] {
    def compare(a: Bus, b: Bus): Int = b.next.head compare a.next.head
  }

  val earliestTime: Int = data.head.toInt

  def busesWithIndex(): Array[(Bus, Int)] =
    data(1)
      .split(",")
      .zipWithIndex
      .filterNot(_._1 == "x")
      .map { case (b, idx) => (Bus(b.toInt), idx) }
}

object Day13_1 extends App with Day13 {
  @tailrec
  def nextBus(time: Int, queue: mutable.PriorityQueue[Bus]): Bus = {
    val bus = queue.dequeue()
    if (bus.next.head > time) bus
    else {
      queue.enqueue(bus.copy(next = bus.next.tail))
      nextBus(time, queue)
    }
  }
  val busQueue = new mutable.PriorityQueue[Bus]()
  busQueue.enqueue(busesWithIndex().map(_._1): _*)
  val bus = nextBus(earliestTime, busQueue)
  println(bus.id * (bus.next.head - earliestTime))
}

object Day13_2 extends App with Day13 {
  val busIdAndIndices = busesWithIndex().map {
    case (bus, index) => (bus.id, index)
  }

  val (step, maxIndex) = busIdAndIndices.maxBy(_._1)

  // BRUTE FORCE!!!
  var time = 0L + step - maxIndex
  println(OffsetDateTime.now())
  var found   = false
  var counter = 0L
  while (!found)
    if (
      busIdAndIndices.forall {
        case (id, index) => (time + index) % id == 0
      }
    ) found = true
    else {
      time = time + step
      counter = counter + 1
      if (counter % 1000000000 == 0) println(time)
    }

  println(time)
  println(OffsetDateTime.now()) // 40 minutes later...
}
