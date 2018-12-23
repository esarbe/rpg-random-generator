package chars.titfortat
package service

import PrisonersDilemma.{Action, PlayerId}

trait PlayerInteraction[F[_], C] {
  def askUser(playerId: PlayerId, context: C): F[Action]
}
