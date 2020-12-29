import scala.annotation.tailrec
import scala.io.Source

trait Day22 {
  val data: List[String] =
    Source
      .fromInputStream(getClass.getResourceAsStream("input_22.txt"))
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

  case class Player(name: String, deck: List[Int])

  val decks: List[Player] = groups.map(g => Player(g.head, g.tail.map(_.toInt)))
  val player1Name: String = decks.head.name

  @tailrec
  final def play1(players: List[Player]): Player = {
    val stillInGame = players.filter(_.deck.nonEmpty)
    if (stillInGame.size == 1) stillInGame.head
    else {
      val playersInOrder = stillInGame.sortBy(_.deck.head).reverse
      val (heads, tails) = playersInOrder.map(l => (l.deck.head, l.deck.tail)).unzip
      val winner         = playersInOrder.head.copy(deck = tails.head ::: heads)
      val rest           = playersInOrder.tail.map(p => p.copy(deck = p.deck.tail))

      play1(winner :: rest)
    }
  }

  final def play2(players: List[Player], previous: Set[Set[Player]] = Set()): Player = {
    val stillInGame = players.filter(_.deck.nonEmpty)
    if (stillInGame.size == 1) stillInGame.head
    else if (previous.contains(players.toSet))
      players.find(_.name == player1Name).getOrElse(throw new IllegalStateException())
    else {
      val topCards = players.map(p => (p.name, p.deck.head)).toMap
      val (winner, rest) = if (players.forall(p => p.deck.size - 1 >= p.deck.head)) {
        val subWinner =
          play2(players.map(p => p.copy(deck = p.deck.tail.take(topCards(p.name)))), previous + players.toSet).name
        val winnerCard     = topCards(subWinner)
        val loserCards     = topCards.removed(subWinner).values.toList
        val playersByName  = players.partition(_.name == subWinner)
        val playersInOrder = playersByName._1 ::: playersByName._2
        val winner         = playersInOrder.head.copy(deck = playersInOrder.head.deck.tail ::: winnerCard :: loserCards)
        val rest           = playersInOrder.tail.map(p => p.copy(deck = p.deck.tail))
        (winner, rest)
      } else {
        val playersInOrder = players.sortBy(_.deck.head).reverse
        val winner =
          playersInOrder.head.copy(deck = playersInOrder.head.deck.tail ::: topCards.values.toList.sorted.reverse)
        val rest = playersInOrder.tail.map(p => p.copy(deck = p.deck.tail))
        (winner, rest)
      }
      play2(winner :: rest, previous + players.toSet)
    }
  }
}

object Day22_1 extends App with Day22 {
  val end = play1(decks)
  println(end.deck.reverse.zipWithIndex.map {
    case (card, idx) => card * (idx + 1)
  }.sum)
}

object Day22_2 extends App with Day22 {
  val end = play2(decks)
  println(end.deck.reverse.zipWithIndex.map {
    case (card, idx) => card * (idx + 1)
  }.sum)
}
