package chars.model

import java.io.File


object HumanSurvivalProject extends App {

  import chars.statistics.HiddenMarkovModel._

  val hmm = buildModel(new File("./src/main/resources/names/german-male.txt"))

  val generated: String = hmm.generate(Seq('\n'))(util.Random.nextLong).mkString

  println(s"Name: $generated")

  def printlnRaw(s: String): Unit = {
    println(s.flatMap {
      case '\n' => "\\n"
      case a => a.toString
    })
  }
}
