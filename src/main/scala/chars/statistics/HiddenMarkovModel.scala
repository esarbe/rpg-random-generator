package chars.statistics

import java.io.File

import scala.annotation.tailrec

object HiddenMarkovModel {


  type State = Seq[Char]
  type HiddenMarkovModel = Map[State, Map[State, Int]]
  type Count = Int


  def buildNGramTransitions(n: Int, line: State): Seq[(State, State)] = {
    val transitions = line.sliding(n).foldLeft(List.empty[(State, State)]) {
      case (acc, init :+ last) =>
        (init, Seq(last)) :: acc
    }

    transitions
  }

  def buildModelFromLines(lines: Traversable[String]): HiddenMarkovModel = {

    val nGramTransitions = for {
      line <- lines.toSeq
      paddedLine  = s"\n$line\n"

      ns <- Range(2, paddedLine.length)
      transitions <- buildNGramTransitions(ns, paddedLine)
    } yield transitions

    nGramTransitions.groupBy(_._1).mapValues { values =>
      values
        .map(_._2)
        .groupBy(s => s)
        .mapValues(_.size)
    }
  }

  def buildModel(source: File): HiddenMarkovModel = {
    val lines =
      io.Source
        .fromFile(source)
        .getLines

    buildModelFromLines(lines.toTraversable)
  }

  implicit class HiddenMarkovModelOps(model: HiddenMarkovModel) {

    def generate(current: State)(seed: Long): State = {

      @tailrec
      def generate(current: State, seed: Long): State = {
        val char = Seq(current.last)
        val weights: Map[State, Double] = model(char).mapValues(_.toDouble)
        val possibleNexts = weights.keys.toSeq
        val rand = chars.random.Generators.randomValuesWithWeights(possibleNexts, weights.apply)
        val (nextSeed, nextChar) = rand(seed)

        if (nextChar == Seq('\n')) current
        else generate(current ++ nextChar, nextSeed)
      }

      generate(current, seed)
    }
  }

}

