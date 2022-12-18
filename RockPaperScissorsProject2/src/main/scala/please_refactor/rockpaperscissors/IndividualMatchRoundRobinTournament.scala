package please_refactor.rockpaperscissors

import benjamingarrett.rockpaperscissorstools.{RPSHistoryBasedPlayer, RPSOutcome, RPSTournament}

class IndividualMatchRoundRobinTournament(numRounds: Int) extends RPSTournament {
  val matchHandler = new MultiRoundHistoryBasedMatch(SingleMatchDecider)
  def playTournament(entrants: List[RPSHistoryBasedPlayer]): Map[(RPSHistoryBasedPlayer,RPSHistoryBasedPlayer),List[RPSOutcome]] = {
    entrants.combinations(2).toList.map(x => (x(0), x(1))).map(p => {p -> matchHandler.playMatch(numRounds)(p._1)(p._2)}).toMap
  }
}
