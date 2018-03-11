package chars.statistics

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, WordSpec}

class HiddenMarkovModelSpec extends WordSpec with Matchers with PropertyChecks {

  import chars.statistics.HiddenMarkovModel._

  "A HMM" should {
    "generate state transitions is has seen before" in {

      forAll { i: Int =>

        val model = HiddenMarkovModel.buildModelFromLines(List("abc", "def", "ghi"))
        val gen = model.generate("\n")(i).mkString.trim

        List("abc", "def", "ghi") should contain(gen)
      }

      forAll { i: Int =>
        val model = HiddenMarkovModel.buildModelFromLines(List("abaratawazagajalapanamacaw"))
        val gen = model.generate("\n")(i).mkString.trim
        gen should startWith("a")
        gen should endWith("w")
        gen shouldNot contain("aa")
      }
    }
  }
}
