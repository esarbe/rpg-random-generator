package chars.titfortat
package service

import PrisonersDilemma.{Action, PlayerId}

trait PlayerInteraction[F[_]] {
  def askForUserAction(playerId: PlayerId, context: IteratedPrisonersDilemma[F]#Context): F[Action]
}
