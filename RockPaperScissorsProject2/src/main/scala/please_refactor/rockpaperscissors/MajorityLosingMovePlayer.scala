package please_refactor.rockpaperscissors

import benjamingarrett.rockpaperscissorstools.{BWins, Paper, RPSHistoryBasedPlayer, RPSMove, RPSOutcome, Rock, Scissors}

class MajorityLosingMovePlayer(playerName: String) extends RPSHistoryBasedPlayer with UnbiasedRandomMove {
  override val playerInfo: String = playerName
  val r = scala.util.Random
  override def playMove(history: List[(RPSMove, RPSOutcome)]): RPSMove = {
    val losingMoves = history.filter(x => x._2 == BWins).map(x => x._1)
    val moveCounts: List[(RPSMove, Int)] = Map(
      Rock -> losingMoves.count(move => move == Rock),
      Paper -> losingMoves.count(move => move == Paper),
      Scissors -> losingMoves.count(move => move == Scissors)
    ).toList.sortWith((m1, m2) => m1._2 > m2._2)
    moveCounts.headOption match {
      case Some(pair) => pair._1
      case None => unbiasedRandomMove(r)
    }
  }
}
