import please_refactor.rockpaperscissors.SingleMatchDecider.beats

class SingleMatchDeciderTests extends MatchDeciderTests {
  "SingleMatchDecider" should "judge rock-paper-scissors moves correctly with valid inputs" in {
    assert(validPairs.map { case (moves, expected) => beats(moves._1)(moves._2) == expected }.forall(_ == true))
  }
}
