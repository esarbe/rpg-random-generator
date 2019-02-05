package chars

import _root_.cats.data.State
import _root_.cats.Applicative

package object random {

  case class Seed(value: Long) {
    def next[F[_]](implicit F: Applicative[F]): F[Seed] = {
      F.pure(Seed(value * 25214903917L + 11L & 281474976710655L))
    }
  }

  type Random[T] = State[Seed, T]
}
