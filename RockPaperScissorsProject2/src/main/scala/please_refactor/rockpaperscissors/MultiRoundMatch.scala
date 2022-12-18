package please_refactor.rockpaperscissors

import benjamingarrett.rockpaperscissorstools.{AWins, BWins, RPSDecider, RPSHistoryBasedPlayer, RPSMatch, RPSMove, RPSOutcome, Tie}

class MultiRoundHistoryBasedMatch(decider: RPSDecider) extends RPSMatch {
  override def playMatch(rounds: Int)(firstPlayer: RPSHistoryBasedPlayer)(secondPlayer: RPSHistoryBasedPlayer): List[RPSOutcome] = {
    def play(
        firstPlayerHistory: List[(RPSMove,RPSOutcome)],
        secondPlayerHistory: List[(RPSMove,RPSOutcome)],
        matchResults: List[RPSOutcome],
        roundsRemaining: Int
    ): List[RPSOutcome] =
      if (roundsRemaining == 0)
        matchResults
      else {
        val firstMove = firstPlayer.playMove(firstPlayerHistory)
        val secondMove = secondPlayer.playMove(secondPlayerHistory)
        val outcome1 = decider.beats(firstPlayer.playMove(firstPlayerHistory))(secondMove) match {
          case Tie => Tie
          case BWins => BWins
          case AWins => AWins
        }
        val outcome2 = decider.beats(firstPlayer.playMove(firstPlayerHistory))(secondMove) match {
          case Tie => Tie
          case BWins => AWins
          case AWins => BWins
        }
        play(
          (firstMove, outcome1) :: firstPlayerHistory,
          (secondMove, outcome2) :: secondPlayerHistory,
          outcome1 :: matchResults,
          roundsRemaining - 1)
      }
    play(List.empty, List.empty, List.empty, rounds)
  }
}
