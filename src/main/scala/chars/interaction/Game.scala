ackage chars.interaction

object RunGame extends App {
  val greedo = new r.game.GreedyPlayer { override def toString = "greedo"}
  val greedo2 = new r.game.GreedyPlayer { override def toString = "greedo2"}
  val fairo = new r.game.CooperatingPlayer { override def toString = "fairo" }
  val fairo2 = new r.game.CooperatingPlayer { override def toString = "fairo2" }

  val r = new Round {
    val game = PrisonersDilemma
  }

  val participants = Set(greedo, greedo2, fairo, fairo2)

  val matchups = participants.toList.combinations(2)

  val f = matchups.collect { case List(left, right) =>
      val g = ??? //r.run(left, right)
  }

  //val payoffs = r.run(participants)

  //println(s"po: $payoffs")
}

trait Game {

  sealed trait Action extends Product with Serializable
  object Action {
    case object Defect extends Action
    case object Cooperate extends Action
  }

  type Payoff = Double

  trait Participant {
    def apply(other: Participant): Action
  }

  def evaluate(actions: Seq[Action]): Action => Double
}


trait Round {
  val game: Game


}


object PrisonersDilemma extends Game {

  def run(left: Participant, right: Participant): Participant => Double = {

    val leftAction = left.apply(right)
    val rightAction = right.apply(left)

    val actionsByParticipant = Map(left-> leftAction, right -> rightAction)

    val payoffForAction = evaluate(actionsByParticipant.values.toSeq)

    actionsByParticipant.mapValues(payoffForAction)
  }


  case class Outcome(participant: Participant, action: Action, payoff: Payoff)

  def evaluate(actions: Seq[Action]): Action => Double = {
    import Action._

    val defectors = actions.count(_ == Defect) / actions.size.toDouble

    {
      case Defect => -0.5 + 2 * defectors
      case Cooperate => -1 + 2 * defectors
    }
  }

  trait GreedyPlayer extends Participant {
    override def apply(other: Participant): Action = Action.Defect
  }

  trait CooperatingPlayer extends Participant {
    override def apply(other: Participant): Action = Action.Cooperate
  }

}
