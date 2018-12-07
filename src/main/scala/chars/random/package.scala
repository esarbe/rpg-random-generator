package chars

package object random {

  type Random[T] = Long => (Long, T)

  implicit class RandomOps[T](val r: Random[T]) extends AnyVal {
    def run(seed: Long): T = r.apply(seed)._2
  }
}
