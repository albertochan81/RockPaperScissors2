package please_refactor.rockpaperscissors

import benjamingarrett.rockpaperscissorstools.{AWins, Paper, RPSHistoryBasedPlayer, RPSMove, RPSOutcome, Rock, Scissors}

class MajorityWinningMovePlayer(playerName: String) extends RPSHistoryBasedPlayer with UnbiasedRandomMove {
  override val playerInfo: String = playerName
  val r = scala.util.Random
  override def playMove(history: List[(RPSMove, RPSOutcome)]): RPSMove = {
    val winningMoves = history.filter(x => x._2 == AWins).map(x => x._1)
    val moveCounts: List[(RPSMove, Int)] = Map(
      Rock -> winningMoves.count(move => move == Rock),
      Paper -> winningMoves.count(move => move == Paper),
      Scissors -> winningMoves.count(move => move == Scissors)
    ).toList.sortWith((m1, m2) => m1._2 > m2._2)
    moveCounts.headOption match {
      case Some(pair) => pair._1
      case None => unbiasedRandomMove(r)
    }
  }
}
