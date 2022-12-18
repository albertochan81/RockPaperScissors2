package please_refactor.rockpaperscissors

import benjamingarrett.rockpaperscissorstools.{Paper, RPSHistoryBasedPlayer, RPSMove, RPSOutcome, Rock, Scissors}

class BiasedRandomMovePlayer(playerName: String, weights: Map[RPSMove, Double]) extends RPSHistoryBasedPlayer {
  override val playerInfo = playerName
  val r = scala.util.Random
  val total = weights.foldLeft(0.0)({ case (acc,(_,weight)) => acc + weight })
  val normalizedWeights = weights.map { case (move, weight) => move -> weight / total }
  override def playMove(history: List[(RPSMove,RPSOutcome)]): RPSMove = biasedRandomMove(r.nextDouble())
  def biasedRandomMove(x: Double): RPSMove = {
    if (0.0 <= x && x < normalizedWeights(Rock)) Rock
    else if (normalizedWeights(Rock) <= x && x < normalizedWeights(Rock) + normalizedWeights(Paper)) Paper
    else Scissors
  }
}
