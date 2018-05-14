package chars.random.model

import chars.model.Humanoid
import chars.random.Random

object HumanoidGen extends Random[Humanoid] {

  import cats.implicits._
  import chars.random.CatsInstances._
  import chars.random.Generators._

  val randomValueCategory = randomEnum[chars.model.CategorizedValue]

  val randomEyeColor = randomEnumWithWeights[Humanoid.Face.Eyes.Eye.Color] {
    case Humanoid.Face.Eyes.Eye.Color.Brown => 2
    case Humanoid.Face.Eyes.Eye.Color.Blue => 1
    case Humanoid.Face.Eyes.Eye.Color.Hazel => 0.5
    case Humanoid.Face.Eyes.Eye.Color.Green => 0.2
  }

  val randomAge = randomEnumWithWeights[Humanoid.Age]{
    case Humanoid.Age.Young => 2
    case Humanoid.Age.Middleaged => 0.4
    case Humanoid.Age.Old => 1
    case Humanoid.Age.Ancient => 0.5
  }

  val randomBodyHeight = randomValueCategory.map(Humanoid.Body.Height)
  val randomBodyWeight = randomValueCategory.map(Humanoid.Body.Weight)
  val randomAthleticism = randomValueCategory.map(Humanoid.Body.Athleticism)
  val randomBody = (randomBodyHeight, randomBodyWeight,  randomAthleticism).mapN(Humanoid.Body.apply)
  val randomFaceShape = randomEnum[Humanoid.Face.Shape]
  val randomEyeShape = randomEnum[Humanoid.Face.Eyes.Eye.Shape]

  val randomEyes = (randomEyeColor, randomEyeShape).mapN(Humanoid.Face.Eyes.Eye.apply)
  val randomFace = (randomFaceShape, randomEyes).mapN(Humanoid.Face.apply)
  val randomHead = (randomFace, randomValueCategory).mapN(Humanoid.Head.apply)
  val randomSex = randomEnumWithWeights[Humanoid.Sex] {
    case Humanoid.Sex.Male => 1.2
    case Humanoid.Sex.Female => 0.8
    case Humanoid.Sex.Indeterminate => 0.1
  }

  val randomRace = randomEnum[Humanoid.Race]

  val randomPerson = (randomSex, randomAge, randomBody, randomHead).mapN(Humanoid.apply)


  override def apply(seed: Long): (Long, Humanoid) = randomPerson(seed)
}
