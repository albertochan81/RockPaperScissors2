package please_refactor.rockpaperscissors

import benjamingarrett.rockpaperscissorstools.{AWins, BWins, Tie, RPSDecider, RPSMove, RPSHistoryBasedPlayer, RPSOutcome, RPSPointsSchema, RPSTournament, RPSTournamentSeason, Paper,Scissors,Rock}

import scala.io.StdIn.readLine

object RockPaperScissorsCmdGame {
  def main(args: Array[String]) = {
    greeting

    print("Enter 1 for live game play. Enter 2 for automatic tournament season: ")
    readLine() match {
      case "1" => liveGamePlay
      case "2" => handleTournamentSeason
      case _ => println("Response not understood.")
    }
    goodbye
  }
  private def greeting = println("Welcome to Rock-Paper-Scissors")
  private def goodbye = println("Thanks for playing Rock-Paper-Scissors")
  private def liveGamePlay = {
    handleOneTurn
    while ( {
      print("Play again? (true/false): ");
      readLine().toBoolean
    }) handleOneTurn
  }
  private def handleOneTurn = {
    print("Enter first move: ")
    val firstMove = readLine()
    print("Enter second move: ")
    val secondMove = readLine()
    val decision = DeciderMapper.beats(firstMove)(secondMove) match {
      case Some(value) => value
      case None => "invalid input given"
    }
    println(s"decision: ${decision}")
  }
  private def handleTournamentSeason = {
    val DTO= JsonParser.go("./src/main/resources/tournamentConfig.json")

    val weights1 = Map[RPSMove, Double](Rock -> SeasonInitialization.getWeight(DTO)(0), Paper -> SeasonInitialization.getWeight(DTO)(1), Scissors -> SeasonInitialization.getWeight(DTO)(2))
    val weights2 = Map[RPSMove, Double](Rock -> SeasonInitialization.getWeight(DTO)(3), Paper -> SeasonInitialization.getWeight(DTO)(4), Scissors -> SeasonInitialization.getWeight(DTO)(5))

    val players = List(
      new LastLosingMovePlayer(SeasonInitialization.getPlayer(DTO)(0)),
      new LastLosingMovePlayer(SeasonInitialization.getPlayer(DTO)(1)),
      new LastWinningMovePlayer(SeasonInitialization.getPlayer(DTO)(2)),
      new MajorityLosingMovePlayer(SeasonInitialization.getPlayer(DTO)(3)),
      new MajorityWinningMovePlayer(SeasonInitialization.getPlayer(DTO)(4)),
      new RandomMovePlayer(SeasonInitialization.getPlayer(DTO)(5)),
      new BiasedRandomMovePlayer(s"${SeasonInitialization.getPlayer(DTO)(5)} ${weights1}", weights1),
      new BiasedRandomMovePlayer(s"${SeasonInitialization.getPlayer(DTO)(6)} ${weights2}", weights2)
    )
    println("Calculating Results... ")
    val numRounds =SeasonInitialization.getRounds(DTO)

    val tournaments = List(
      new IndividualMatchRoundRobinTournament(numRounds),
      new IndividualMatchRoundRobinTournament(numRounds),
      new IndividualMatchRoundRobinTournament(numRounds),
      new IndividualMatchRoundRobinTournament(numRounds)
    )
    val seasonResults: Map[RPSHistoryBasedPlayer, Int] = MixedTournamentSeason.handleTournamentSeason(players)(tournaments)
    println(s"Summary of tournament season:\n${seasonResults}")
    for (r <- seasonResults)
      println(s"Player: ${r._1.playerInfo}   Points: ${r._2}")

  }
}
