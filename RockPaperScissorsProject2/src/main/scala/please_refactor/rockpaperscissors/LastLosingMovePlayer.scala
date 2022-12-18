package please_refactor.rockpaperscissors

import benjamingarrett.rockpaperscissorstools.{BWins, RPSHistoryBasedPlayer, RPSMove, RPSOutcome}

class LastLosingMovePlayer(playerName: String) extends RPSHistoryBasedPlayer with UnbiasedRandomMove {
  override val playerInfo: String = playerName
  val r = scala.util.Random
  override def playMove(history: List[(RPSMove, RPSOutcome)]): RPSMove =
    history.find(x => x._2 == BWins) match {
      case Some(previousGame) => previousGame._1
      case None => unbiasedRandomMove(r)
    }
}
