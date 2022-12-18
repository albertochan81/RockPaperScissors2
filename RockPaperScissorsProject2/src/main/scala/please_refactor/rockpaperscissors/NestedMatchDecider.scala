package please_refactor.rockpaperscissors

import benjamingarrett.rockpaperscissorstools.{AWins, BWins, Paper, RPSDecider, RPSMove, RPSOutcome, Rock, Scissors, Tie}

object NestedMatchDecider extends RPSDecider {
  override def beats(firstMove: RPSMove)(secondMove: RPSMove): RPSOutcome =
    firstMove match {
      case Rock => secondMove match {
        case Rock => Tie
        case Paper => BWins
        case Scissors => AWins
      }
      case Paper => secondMove match {
        case Rock => AWins
        case Paper => Tie
        case Scissors => BWins
      }
      case Scissors => secondMove match {
        case Rock => BWins
        case Paper => AWins
        case Scissors => Tie
      }
    }
}
