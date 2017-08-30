package chars.model

import java.io.File

object HumanSurvivalProject extends App {

  type State = Seq[Char]
  type HMM = Map[State, Map[State, Int]]
  type Count = Int

  def buildHMM(source: File): HMM = {
    val lines = io.Source.fromFile(source).map(_.toLower)

    val history = 2
    type State = Map[Seq[Char], Int]
    val init = Map.empty[Seq[Char], Int].withDefaultValue(0)

    lines.sliding(history, 1).foldLeft(init) { case (acc, curr) =>

      if (curr.size < history) acc // don't consider state changes with small history
      else {
        val count = acc(curr)
        acc.updated(curr, count + 1)
      }
    }
  }

  def generateFromHMM(hmm: HMM)(char: Char, seq: String)(seed: Long): String = {
    val weights: Map[Char, Double] = hmm
      .toList
      .filter { _._1.headOption.contains(char) }
      .map { case (key, value) => (key.tail.head, value) }
      .groupBy { _._1 }
      .mapValues { _.map(_._2).sum.toDouble }

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
