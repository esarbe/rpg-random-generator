package chars

import org.scalatest.FunSuite

import scala.collection.immutable

object SquareProbabilities extends App {

  val maxDice = 18
  val numAttempts = 100000

  val counts = for {
    dice <- 1 to maxDice
    _ <- 1 to numAttempts
  } yield (dice, roll(dice))

  // Int => Int => Int
  val successesByDieCount = counts
    .groupBy(_._1)
    .mapValues { _.groupBy(_._2).mapValues(_.length) }


  val maxSuccesses = math.min(successesByDieCount.map(_._2.values.max).max, 12)
  val successesHeader = (0 to maxSuccesses).mkString(";")

  println(s"num dice; mean; $successesHeader")

  (1 to maxDice).foreach { i =>

    val numEvents = successesByDieCount(i).values.sum
    val total = successesByDieCount(i).map { case (successes, events) => successes * events }.sum.toDouble

    print(s"$i;${total / numEvents};")

    (0 to maxSuccesses).foreach { s =>

      val is = successesByDieCount.get(i).flatMap(_.get(s)).getOrElse(0)
      print(s"${is.toDouble / numAttempts};")
    }
    print("\n")
  }

  (1 to maxDice).foreach { i =>

    val numEvents = successesByDieCount(i).values.sum
    val total = successesByDieCount(i).map { case (successes, events) => successes * events }.sum.toDouble

    print(s"$i;${total / numEvents};")

    val cumulativeSuccesses =
      successesByDieCount(i).toList.sortBy(_._1).reverse.foldLeft((0, List.empty[(Int, Int)])) {
        case ((accTotal, accu), (key, value)) =>

          val newAccTotal = accTotal + value
          val newEntry = (key, newAccTotal)

          (newAccTotal, newEntry :: accu)
      }._2.toMap


    (0 to maxSuccesses).foreach { s =>

      val is = cumulativeSuccesses.getOrElse(s, 0)
      print(s"${is.toDouble / numAttempts};")
    }
    print("\n")
  }



  def roll(n: Int): Int = {

    val success: Int => Boolean = _ > 2

    val throws = (1 to n).map(_ => scala.util.Random.nextInt(6))

    val successes = throws.count(success)
    val numRethrows = throws.count(_ == 5)
    val rethrowSuccesses =
      if (numRethrows > 0) roll(numRethrows)
      else 0

    successes +  rethrowSuccesses
  }
}
