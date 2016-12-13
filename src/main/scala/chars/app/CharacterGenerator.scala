package chars.app

import chars.random.model.HumanoidGen

import scala.scalajs.js.JSApp

object CharacterGenerator extends JSApp {
  def main(): Unit = {
    println(HumanoidGen(util.Random.nextLong))
  }
}
