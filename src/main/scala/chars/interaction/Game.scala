package chars.interaction
import cats.implicits._

object RunGame extends App {
  val r = new Round {
    val game = PrisonersDilemma
  }

  val greedo = new GreedyPlayer { override def toString() = "greedo"}
  val greedo2 = new GreedyPlayer { override def toString() = "greedo2"}
  val fairo = new CooperatingPlayer { override def toString = "fairo" }
  val fairo2 = new CooperatingPlayer { override def toString = "fairo2" }

  val participants = Set(greedo, greedo2, fairo, fairo2)

  val payoffs = r.run(participants)

  println(s"po: $payoffs")
}

trait Game {
  type Action
  type Payoff = Double

  def actions: Set[Action]
  def evaluate(actions: Seq[Action]): Action => Payoff
}

trait Participant {
  def apply(game: Game, participants: Set[Participant]): game.Action
}

trait CategoricalPlayer extends Participant {
  override def apply(game: Game, participants: Set[Participant]): game.Action = {
    val everyoneDoesIt = game.actions.map { action =>
      val ifEveryOneDoes: game.Action => game.Payoff = game.evaluate(participants.toSeq.map(_ => action))
      (action, ifEveryOneDoes(action))
    }.toSeq.sortBy(_._2)

    everyoneDoesIt.reverse.head._1
  }
}

trait GreedyPlayer extends Participant {
  override def apply(game: Game, participants: Set[Participant]): game.Action = game.actions.head
}

trait CooperatingPlayer extends Participant {
  override def apply(game: Game, participants: Set[Participant]): game.Action = game.actions.last
}

trait Round {
  val game: Game

  def run(participants: Set[Participant]): Participant => Game#Payoff = {

  val actionsByParticipant =
    (participants zip participants)
      .toMap
      .mapValues { _.apply(game, participants)  }

  val payoffForAction = game.evaluate(actionsByParticipant.values.toSeq)

  actionsByParticipant.mapValues(payoffForAction)
  }
}


object PrisonersDilemma extends Game {

  sealed trait Action extends Product with Serializable
  private object Action {
    case object Defect extends Action
    case object Cooperate extends Action
  }

  import Action._

  val actions = Set(Action.Defect, Action.Cooperate)

  def evaluate(actions: Seq[Action]): Action => Payoff = {
    val defectors = actions.count(_ == Defect) / actions.size.toDouble

    {
      case Defect => -0.5 + 2 * defectors
      case Cooperate => -1 + 2 * defectors
    }
  }
}
