package chars.titfortat

import cats._
import cats.implicits._
import chars.random._
import chars.titfortat.PrisonersDilemma.Action.{Cooperate, Defect}
import chars.titfortat.PrisonersDilemma.{Action, Payoffs, PlayerId, Score}
import chars.titfortat.service.PlayerInteraction


class TitForTat[M[_]](
  val game: IteratedPrisonersDilemma[M],
  generator: Generator[M],
  interaction: PlayerInteraction[M]
)(implicit M: Monad[M]) {

  import game._

  val interactive = new Strategy {
    override def chose(context: Context, player: Player, opponent: PlayerId): M[Action] =
      interaction.askForUserAction(player.id, context)

    override def toString: String = s"Human Player"
  }

  val payoffs: Payoffs = Map(
    (Cooperate, Cooperate) -> (3.0,3.0),
    (Cooperate, Defect) -> (0, 4),
    (Defect, Cooperate) -> (4, 0),
    (Defect, Defect) -> (1,1)
  )

  def buildPlayers(distribution: Seq[(Strategy, Int)]): M[Set[Player]] = {
    val strategies = distribution.flatMap { case (strategy, number) => List.fill(number)(strategy) }

    val players =
      strategies
        .zipWithIndex
        .map { case (s, i) => buildPlayer(PlayerId(i), s)}
        .toSet

    M.pure(players)
  }


  def buildPairings(players: Set[Player]): M[Seq[Pairing]] = {

    val ids = players.toList

    ids.traverse { id =>
        val opponent = generator.oneOf(ids.filterNot(_ == id):_*)
        opponent.map((id, _))
    }.map(_.toSeq)
  }


  def buildRoundsPairings(rounds: Int)(players: Set[Player]): M[Seq[Pairings]] =
    List.fill(rounds)(players).traverse(buildPairings).map(_.toSeq)


  def runGame(players: Set[Player], rounds: Int): M[Map[PlayerId, Score]] = {
    buildRoundsPairings(rounds)(players)
      .flatMap { pairingRounds =>

        val endStateM = pairingRounds.foldLeft(M.pure(initialState)) { case (state, pairings) =>
          pairings.foldLeft(state){ case (stateM, pairing) => stateM.flatMap(runPairing(payoffs, _, pairing)) }
        }

        endStateM.map { endstate =>

          players.map { participant =>
            participant.id -> endstate.scores(participant.id)
          }.toMap
        }
      }
    }
}
