package chars.random.model

import cats.Monad
import chars.model._
import chars.model.human.face.{Eye, Eyes}
import chars.model.human.{Age, Head}
import chars.random.Generator

class Human[M[_]: Monad](generator: Generator[M]) {

  import cats.implicits._

  val randomValueCategory = generator.randomEnum[chars.model.CategorizedValue]

  val randomEyeColor = generator.randomEnumWithWeights[human.face.eye.Color] {
    case human.face.eye.Color.Brown => 7
    case human.face.eye.Color.Blue => 1
    case human.face.eye.Color.Hazel => 0.5
    case human.face.eye.Color.Green => 0.2
    case human.face.eye.Color.Grey => 0.1
  }

  val randomAge = generator.randomDouble.map(i => Age(i * 120 + 14))

  val randomBodyHeight: M[body.Height] = randomValueCategory.map(body.Height)
  val randomBodyWeight: M[body.Weight] = randomValueCategory.map(body.Weight)
  val randomAthleticism: M[body.Athleticism] = randomValueCategory.map(body.Athleticism)
  val randomBody = for {
    height <- randomBodyHeight
    weight <- randomBodyWeight
    athleticism <- randomAthleticism
  } yield Body(height, weight, athleticism)


  val randomFaceShape = generator.randomEnum[human.face.Shape]
  val randomEyeShape = generator.randomEnum[human.face.eye.Shape]

  val randomEye: M[Eye] =
    for {
      eyeColor <- randomEyeColor
      eyeShape <- randomEyeShape
    } yield human.face.Eye(eyeColor, eyeShape)

  val randomSameEyes: M[Eyes] =
    for {
      eye <- randomEye
    } yield human.face.Eyes.SameEyes(eye)


  val randomDifferentEyes: M[Eyes] =
    for {
      leftEye <- randomEye
      rightEye <- randomEye
    } yield human.face.Eyes.DifferentEyes(leftEye, rightEye)


  val randomEyes =
    generator.oneOf[M[Eyes]](randomDifferentEyes, randomSameEyes).flatten

  val randomFace =
    for {
      shape <- randomFaceShape
      eyes <- randomEyes
    } yield human.Face(shape, eyes)

  val randomHead =
    for {
      face <- randomFace
      size <- randomValueCategory
    } yield human.Head(face, size)

  val randomSex = generator.randomEnumWithWeights[Sex] {
    case Sex.Male => 1.1
    case Sex.Female => 0.9
    case Sex.Indeterminate => 0.1
  }

  val randomHuman = for {
    sex <- randomSex
    age <- randomAge
    body <- randomBody
    head <-  randomHead
  } yield chars.model.Human(sex, age, body, head)

  def buildGenerator(
      randomSex: M[Sex] = randomSex,
      randomAge: M[Age] = randomAge,
      randomBody: M[Body] = randomBody,
      randomHead: M[Head] = randomHead
  ): M[chars.model.Human] =
    for {
      sex <- randomSex
      age <- randomAge
      body <- randomBody
      head <-  randomHead
    } yield chars.model.Human(sex, age, body, head)
}
