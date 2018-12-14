package chars.statistics

import java.io.File

import chars.random.Seed

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
      scala.io.Source
        .fromFile(source)
        .getLines

    buildModelFromLines(lines.toTraversable)
  }

  implicit class HiddenMarkovModelOps(model: HiddenMarkovModel) {

    def generate(current: State)(seed: Seed): State = {

      @tailrec
      def generate(current: State, seed: Seed): State = {
        val char = Seq(current.last)
        val weights: Map[State, Double] = model(char).mapValues(_.toDouble)
        val possibleNexts = weights.keys.toSeq
        val rand = chars.random.Generator.randomValuesWithWeights(possibleNexts, weights.apply)
        val (nextSeed, nextChar) = rand.run(seed).value

        if (nextChar == Seq('\n')) current
        else generate(current ++ nextChar, nextSeed)
      }

      generate(current, seed)
    }
  }

}


