package chars.random.model

import chars.model.Humanoid
import chars.random.Random

object HumanoidGen extends Random[Humanoid] {

  import cats.implicits._
  import chars.random.implicits._

  val randomValueCategory = randomEnum[Humanoid.CategorizedValue]
  val randomColor = randomEnum[Humanoid.Face.Eyes.Eye.Color]
  val randomAge = randomEnum[Humanoid.Age]
  val randomBodyHeight = randomValueCategory.map(Humanoid.Body.Height)
  val randomBodyWeight = randomValueCategory.map(Humanoid.Body.Weight)
  val randomAthleticism = randomValueCategory.map(Humanoid.Body.Athleticism)
  val randomBody = (randomBodyHeight |@| randomBodyWeight |@| randomAthleticism).map(Humanoid.Body.apply)
  val randomFaceShape = randomEnum[Humanoid.Face.Shape]
  val randomEyeShape = randomEnum[Humanoid.Face.Eyes.Eye.Shape]
  val randomEyeColor = randomEnum[Humanoid.Face.Eyes.Eye.Color]

  val randomEyes = (randomEyeColor |@| randomEyeShape).map(Humanoid.Face.Eyes.Eye.apply)
  val randomFace = (randomFaceShape |@| randomEyes).map(Humanoid.Face.apply)
  val randomHead = (randomFace |@| randomValueCategory).map(Humanoid.Head.apply)
  val randomSex = randomEnum[Humanoid.Sex]

  val randomRace = randomEnum[Humanoid.Race]

  val randomPerson = (randomSex |@| randomAge |@| randomRace |@| randomBody |@| randomHead).map(Humanoid.apply)


  override def apply(seed: Long): (Long, Humanoid) = randomPerson(seed)
}
