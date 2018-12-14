package chars

import io.estatico.newtype.macros.newtype
import _root_.cats.data.State

package object random {

  @newtype case class Seed(value: Long) {
    def next: Seed = {
      Seed(value * 25214903917L + 11L & 281474976710655L)
    }
  }

  type Random[T] = State[Seed, T]

  implicit class RandomOps[T](val r: Random[T]) extends AnyVal {

  }
}
