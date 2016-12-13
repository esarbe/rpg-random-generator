package chars

import org.scalatest.FunSuite

class CharacterSuite extends FunSuite {

  import chars.random.model.HumanoidGen

  test("random person") {
    println(HumanoidGen(3))
  }

}
