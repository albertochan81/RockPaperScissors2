package please_refactor.rockpaperscissors

import please_refactor.rockpaperscissors.JsonParser.SampleDTO

object SeasonInitialization {

  def getFile(file: SampleDTO) =file

  def getTournaments (DTo : SampleDTO)= {
  DTo match {
      case SampleDTO(tournament, _, _, _, _) => tournament.toInt
    }
  }

  def getRounds(DTo: SampleDTO) = {
    DTo match {
      case SampleDTO(_, rounds, _, _,_) => rounds.toInt
    }
  }

  def getPlayer(DTo: SampleDTO) = {
    DTo match {
      case SampleDTO(_, _, player, _,_) => player
    }
  }

  def getPlayerType(DTo: SampleDTO) = {
    DTo match {
      case SampleDTO(_, _, _, playerType,_) => playerType
    }
  }

  def getWeight(DTo: SampleDTO) = {
    DTo match {
      case SampleDTO(_, _, _,_, weights) => weights
    }
  }

}
