package chars.random

object Person {

  import chars.model.Person._
  import chars.random.implicits.randomEnum
  import cats.implicits._
  import chars.random.implicits._

  val randomValueCategory = randomEnum[CategorizedValue]
  val randomColor = randomEnum[Color]
  val randomAge = randomEnum[Age]
  val randomHeight = randomValueCategory.map(Height)
  val randomWeight = randomValueCategory.map(Weight)
  val randomSex = randomEnum[Sex]
  val randomPerson = (randomSex |@| randomAge |@| randomHeight |@| randomWeight).map(chars.model.Person.Person)
}
