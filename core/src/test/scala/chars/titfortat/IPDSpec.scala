package chars.titfortat

import cats.Id
import chars.titfortat.PrisonersDilemma.Action.{Cooperate, Defect}
import chars.titfortat.PrisonersDilemma.{Action, Payoff, Payoffs, PlayerId}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, WordSpec}

class IPDSpec extends WordSpec with Matchers with GeneratorDrivenPropertyChecks {
  val IPD = new IteratedPrisonersDilemma[Id]
  import IPD._
  val payoffs = Map.empty[(Action, Action), (Payoff, Payoff)].withDefaultValue((0d, 0d))
  val tftId = PlayerId(1)
  val defectorId = PlayerId(2)
  val tftPlayer = IPD.buildPlayer(tftId, titForTat)
  val defectorPlayer = IPD.buildPlayer(defectorId, defect)
  val context = buildContext(initialState, payoffs, tftId, defectorId)


  "a tit-for-tat player" should {
    val context = buildContext(initialState, payoffs,  tftId, defectorId)

    "chose to cooperate in new pairing" in {
      val chosenAction = titForTat.chose(context, tftPlayer, defectorId)
      chosenAction shouldBe Cooperate
    }

    "chose the last action of the opponent for existing parings" in {
      val context = buildContext(initialState.copy(history = Map((defectorId, tftId) -> Defect)), payoffs, tftId, defectorId)
      val chosenAction = titForTat.chose(context, tftPlayer, defectorId)
      chosenAction shouldBe Defect
    }
  }


  "a game of iterated prisoners' dilemma" should {
    "should start with an empty state" in {
      initialState.scores shouldBe Map.empty
      initialState.history shouldBe Map.empty

    }

    "build an empty context for an empty state" in {
      val context = buildContext(initialState, payoffs, tftId, defectorId)
      context.state shouldBe initialState

      forAll { (playerIdNumber: Int, otherIdNumber: Int) =>
        context.getLastMove(PlayerId(playerIdNumber), PlayerId(otherIdNumber)) shouldBe None

        context.getScore(PlayerId(playerIdNumber)) shouldBe 0l
      }
    }

    "create an upsted state after a run with the correct score assigned" in {
      val payoffs: Payoffs = Map(
        (Cooperate, Cooperate) -> (3.0,3.0),
        (Cooperate, Defect) -> (0, 4),
        (Defect, Cooperate) -> (4, 0),
        (Defect, Defect) -> (1,1)
      )

      val state0 = runPairing(payoffs, initialState, (tftPlayer, defectorPlayer))
      state0.scores(defectorId) shouldBe 4d
      state0.scores(tftPlayer.id) shouldBe 0
    }

  }


}
