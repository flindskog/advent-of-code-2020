import scala.collection.immutable.BitSet
import scala.io.Source
import scala.util.matching.Regex

trait Day14 {
  val data: LazyList[String] =
    Source
      .fromInputStream(getClass.getResourceAsStream("input_14.txt"))
      .getLines()
      .map(_.trim)
      .to(LazyList)

  val mask: Regex = """^mask = ([01X]{36})$""".r
  val mem: Regex  = """^mem\[(\d+)] = (\d+)$""".r

  case class Mask(zeros: BitSet, ones: BitSet) {
    val floating: BitSet           = (Mask.allBits diff zeros) diff ones
    def applyTo(value: Long): Long = (value | ones.toBitMask(0)) & ~zeros.toBitMask(0)
    def applyWithFloating(value: Long): List[Long] =
      (0 to floating.size)
        .flatMap(i => floating.toList.combinations(i))
        .map { onesList =>
          val ones = BitSet(onesList: _*)
          val mask = Mask(floating diff ones, ones)
          mask.applyTo(value | this.ones.toBitMask(0))
        }
        .toList
  }

  object Mask {
    def allBits: BitSet = BitSet(0 to 35: _*)
    def empty: Mask     = Mask(BitSet(), BitSet())
    def apply(mask: String): Mask = {
      val (zeroBits, oneBits) = mask.reverse.toCharArray.zipWithIndex.foldLeft((BitSet(), BitSet())) {
        case ((zeroBits, oneBits), (c, i)) =>
          c match {
            case '0' => (zeroBits + i, oneBits)
            case '1' => (zeroBits, oneBits + i)
            case _   => (zeroBits, oneBits)
          }
      }
      Mask(zeroBits, oneBits)
    }
  }

  case class Mem(address: Long, value: Long)

  def applyMapping(updateMemoryFn: (Map[Long, Long], Mem, Mask) => Map[Long, Long]): Map[Long, Long] =
    data.map {
      case mask(bits)          => Mask(bits)
      case mem(address, value) => Mem(address.toLong, value.toInt)
    }.foldLeft((Map[Long, Long](), Mask.empty)) {
      case ((memory, mask), i) =>
        i match {
          case m: Mask => (memory, m)
          case m: Mem  => (updateMemoryFn(memory, m, mask), mask)
        }

    }._1
}

object Day14_1 extends App with Day14 {
  val memory = applyMapping {
    case (memory, mem, mask) => memory.updated(mem.address, mask.applyTo(mem.value))
  }
  println(memory.values.sum)
}

object Day14_2 extends App with Day14 {
  val memory = applyMapping {
    case (memory, mem, mask) =>
      mask.applyWithFloating(mem.address).foldLeft(memory) { (memory, address) =>
        memory.updated(address, mem.value)
      }
  }
  println(memory.values.sum)
}
