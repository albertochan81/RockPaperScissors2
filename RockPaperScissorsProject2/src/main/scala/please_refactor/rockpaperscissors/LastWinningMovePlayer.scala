package please_refactor.rockpaperscissors

import benjamingarrett.rockpaperscissorstools.{AWins, Paper, RPSHistoryBasedPlayer, RPSMove, RPSOutcome, Rock, Scissors}

class LastWinningMovePlayer(playerName: String) extends RPSHistoryBasedPlayer with UnbiasedRandomMove {
  override val playerInfo: String = playerName
  val r = scala.util.Random
  override def playMove(history: List[(RPSMove, RPSOutcome)]): RPSMove =
    history.find(x => x._2 == AWins) match {
      case Some(previousGame) => previousGame._1
      case None => unbiasedRandomMove(r)
    }
}
