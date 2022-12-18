import org.scalatest.flatspec.AnyFlatSpec
import benjamingarrett.rockpaperscissorstools.{AWins, BWins, Paper, Rock, Scissors, Tie}

class MatchDeciderTests extends AnyFlatSpec {
  val validPairs = Map(
    (Rock, Rock) -> Tie,
    (Rock, Scissors) -> AWins,
    (Rock, Paper) -> BWins,
    (Scissors, Scissors) -> Tie,
    (Scissors, Paper) -> AWins,
    (Scissors, Rock) -> BWins,
    (Paper, Paper) -> Tie,
    (Paper, Rock) -> AWins,
    (Paper, Scissors) -> BWins
  )
}
