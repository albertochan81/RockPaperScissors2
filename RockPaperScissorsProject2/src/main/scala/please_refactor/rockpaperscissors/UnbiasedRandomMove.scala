package please_refactor.rockpaperscissors

import benjamingarrett.rockpaperscissorstools.{Paper, Rock, Scissors}

trait UnbiasedRandomMove {
  val unbiasedRandomMove = (r: scala.util.Random) => r.nextInt(3) match {
    case 0 => Rock
    case 1 => Paper
    case 2 => Scissors
  }
}
