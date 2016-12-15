package chars.app

import chars.random.model.HumanoidGen

object CharacterGenerator extends App {
  println(HumanoidGen(util.Random.nextLong))
}
