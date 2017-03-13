package chars.util

import chars.random.Random

class Tester {
  def gen[T](r: Random[T], nr: Int)(implicit ev: Ordering[T]): (T, T) = {

    val numbers = (0 to nr).map { _ =>
      val seed = util.Random.nextLong
      val (_, v) = r(seed)
      v
    }
    val min = numbers.min
    val max = numbers.max

    (min, max)
  }
}
