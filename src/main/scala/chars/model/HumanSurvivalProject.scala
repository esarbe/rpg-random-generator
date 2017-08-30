package chars.model

import java.io.File

import chars.model.HumanSurvivalProject.State

import scala.collection.immutable

object HumanSurvivalProject extends App {

  type State = Seq[Char]
  type HMM = Map[State, Map[State, Int]]
  type Count = Int

  def buildHMM(source: File): HMM = {
    val lines =
      io.Source
        .fromFile(source)
        .getLines
        .map(s => s"\n$s\n".toLowerCase)

    def buildNGramTransitions(n: Int, line: State): Seq[(State, State)] = {
      val transitions = line.sliding(n).foldLeft(List.empty[(State, State)]) {
        case (acc, init :+ last) =>
          (init, Seq(last)) :: acc
      }

      transitions
    }

    val nGramTransitions: Seq[(State, State)] = for {
      line <- lines.toSeq
      ns <- Range(2, line.length)
      transitions <- buildNGramTransitions(ns, line)
    } yield transitions

    nGramTransitions.groupBy(_._1).mapValues { values =>
      values
        .map(_._2)
        .groupBy(s => s)
        .mapValues(_.size)
    }
  }

  def generateFromHMM(hmm: HMM)(current: State)(seed: Long): State = {

    val char = Seq(current.last)

    val weights: Map[State, Double] = hmm.apply(char).mapValues(_.toDouble)

    val possibleNexts = weights.keys.toSeq

    val rand = chars.random.implicits.randomValuesWithWeights(possibleNexts, weights.apply)

    val (nextSeed, nextChar) = rand.apply(seed)

    if (nextChar == Seq('\n')) current
    else generateFromHMM(hmm)(current ++ nextChar)(nextSeed)
  }

  val hmm = buildHMM(new File("./src/main/resources/names/german-male.txt"))


  val generated: String = generateFromHMM(hmm)(Seq('\n'))(util.Random.nextLong).mkString

  println(s"Name: $generated")

  def printlnRaw(s: String): Unit = {
    println(s.flatMap {
      case '\n' => "\\n"
      case a => a.toString
    })
  }
}
