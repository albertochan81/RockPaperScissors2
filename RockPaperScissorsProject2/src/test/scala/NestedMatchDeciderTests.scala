import please_refactor.rockpaperscissors.NestedMatchDecider.beats

class NestedMatchDeciderTests extends MatchDeciderTests {
  "NestedMatchDecider" should "judge rock-paper-scissors moves correctly with valid inputs" in {
    assert(validPairs.map { case (moves, expected) => beats(moves._1)(moves._2) == expected }.forall(_ == true))
  }
}
