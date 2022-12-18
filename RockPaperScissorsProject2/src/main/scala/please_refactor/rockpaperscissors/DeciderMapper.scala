package please_refactor.rockpaperscissors

import benjamingarrett.rockpaperscissorstools.{AWins, BWins, Paper, RPSOutcome, Rock, Scissors, Tie}

object DeciderMapper {
  val moves = Map("rock" -> Rock, "paper" -> Paper, "scissors" -> Scissors)
  val outcomes = Map[RPSOutcome,String](AWins -> "true", BWins -> "false", Tie -> "undecided")
  def beats(move1: String)(move2: String): Option[String] = {
    if (moves.contains(move1) && moves.contains(move2))
      Some(outcomes(NestedMatchDecider.beats(moves(move1))(moves(move2))))
    else
      None
  }
}
