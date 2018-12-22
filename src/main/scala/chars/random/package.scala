package chars

import io.estatico.newtype.macros.newtype
import _root_.cats.data.{State, StateT}
import _root_.cats.Applicative

package object random {

  @newtype case class Seed(value: Long) {
    def next[F[_]](implicit F: Applicative[F]): F[Seed] = {
      F.pure(Seed(value * 25214903917L + 11L & 281474976710655L))
    }
  }

  type Random[T] = State[Seed, T]
}
