trait Day25 {
  val cardPublicKey = 10212254L
  val doorPublicKey = 12577395L

  def transform(subject: Long, v: Long): Long =
    (v * subject) % 20201227L

  def encrypt(subject: Long, value: Long = 1): LazyList[Long] =
    value #:: encrypt(subject, transform(subject, value))

  def getLoopSize(subject: Long, publicKey: Long): Int = {
    val rounds = encrypt(subject)
    rounds.takeWhile(_ != publicKey).size
  }

  val cardLoopSize: Int = getLoopSize(7, cardPublicKey)

  val doorLoopSize: Int = getLoopSize(7, doorPublicKey)

  val e1: Long = encrypt(doorPublicKey).drop(cardLoopSize).head
  val e2: Long = encrypt(cardPublicKey).drop(doorLoopSize).head
}

object Day25_1 extends App with Day25 {
  println(e1)
  println(e2)
}
