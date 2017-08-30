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

    val history = 2
    type State = Map[Seq[Char], Int]
    val init = Map.empty[Seq[Char], Int].withDefaultValue(0)

    lines.sliding(history, 1).foldLeft(init) { case (acc, curr) =>

    val nGramTransitions: Seq[(State, State)] = for {
      line <- lines.toSeq
      ns <- Range(2, line.length)
      transitions <- buildNGramTransitions(ns, line)
    } yield transitions

    }
  }

  def generateFromHMM(hmm: HMM)(current: State)(seed: Long): State = {

    val char = Seq(current.last)

    val weights: Map[State, Double] = hmm.apply(char).mapValues(_.toDouble)

    val possibleNexts = weights.keys.toSeq

    val rand = chars.random.implicits.randomValuesWithWeights(possibleNexts, weights.apply)

    val (nextSeed, nextChar) = rand.apply(seed)

    if (nextChar == '\n') seq
    else generateFromHMM(hmm)(nextChar, seq + nextChar)(nextSeed)
  }

  val hmm = buildHMM(new File("./src/main/resources/names/german-female.txt"))


  val generated: String = generateFromHMM(hmm)('\n', "")(util.Random.nextLong)

  print(generated)

  def printlnRaw(s: String): Unit = {
    println(s.flatMap {
      case '\n' => "\\n"
      case a => a.toString
    })
  }
}
