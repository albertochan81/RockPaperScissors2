package please_refactor.rockpaperscissors

import benjamingarrett.rockpaperscissorstools.{RPSHistoryBasedPlayer, RPSMove, RPSOutcome}

class RandomMovePlayer(playerName: String) extends RPSHistoryBasedPlayer with UnbiasedRandomMove {
  override val playerInfo: String = playerName
  val r = scala.util.Random
  override def playMove(history: List[(RPSMove, RPSOutcome)]): RPSMove = unbiasedRandomMove(r)
}
