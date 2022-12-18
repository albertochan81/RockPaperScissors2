package please_refactor.rockpaperscissors

import benjamingarrett.rockpaperscissorstools.{AWins, BWins, Paper, RPSDecider, RPSMove, RPSOutcome, Rock, Scissors, Tie}

object SingleMatchDecider extends RPSDecider {
  override def beats(firstMove: RPSMove)(secondMove: RPSMove): RPSOutcome =
    (firstMove, secondMove) match {
      case (Rock, Rock) => Tie
      case (Rock, Paper) => BWins
      case (Rock, Scissors) => AWins
      case (Paper, Rock) => AWins
      case (Paper, Paper) => Tie
      case (Paper, Scissors) => BWins
      case (Scissors, Rock) => BWins
      case (Scissors, Paper) => AWins
      case (Scissors, Scissors) => Tie
    }
}
