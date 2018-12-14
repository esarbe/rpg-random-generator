package chars.random.model

import cats.Eval
import cats.data.IndexedStateT
import chars.random.{Generator, Random}
import chars.model._
import chars.model.human.face.Eyes.SameEyes
import chars.model.human.{Age, Head}
import chars.model.human.face.{Eye, Eyes}
import chars.random

object Human {

  import cats.implicits._
  import chars.random.Generator._

  val randomValueCategory = randomEnum[chars.model.CategorizedValue]

  val randomEyeColor = randomEnumWithWeights[human.face.eye.Color] {
    case human.face.eye.Color.Brown => 7
    case human.face.eye.Color.Blue => 1
    case human.face.eye.Color.Hazel => 0.5
    case human.face.eye.Color.Green => 0.2
    case human.face.eye.Color.Grey => 0.1
  }

  val randomAge = randomDouble.map(i => Age(i * 120 + 14))

  val randomBodyHeight: Random[body.Height] = randomValueCategory.map(body.Height)
  val randomBodyWeight: Random[body.Weight] = randomValueCategory.map(body.Weight)
  val randomAthleticism: Random[body.Athleticism] = randomValueCategory.map(body.Athleticism)
  val randomBody = for {
    height <- randomBodyHeight
    weight <- randomBodyWeight
    athleticism <- randomAthleticism
  } yield Body(height, weight, athleticism)


  val randomFaceShape = randomEnum[human.face.Shape]
  val randomEyeShape = randomEnum[human.face.eye.Shape]

  val randomEye: Random[Eye] =
    for {
      eyeColor <- randomEyeColor
      eyeShape <- randomEyeShape
    } yield human.face.Eye(eyeColor, eyeShape)

  val randomSameEyes: Random[Eyes] =
    for {
      eye <- randomEye
    } yield human.face.Eyes.SameEyes(eye)


  val randomDifferentEyes: Random[Eyes] =
    for {
      leftEye <- randomEye
      rightEye <- randomEye
    } yield human.face.Eyes.DifferentEyes(leftEye, rightEye)


  val randomEyes =
    Generator.oneOf[Random[Eyes]](randomDifferentEyes, randomSameEyes).flatten

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

  val randomSex = randomEnumWithWeights[Sex] {
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
      randomSex: Random[Sex] = randomSex,
      randomAge: Random[Age] = randomAge,
      randomBody: Random[Body] = randomBody,
      randomHead: Random[Head] = randomHead
  ) : Random[Human] =
    for {
      sex <- randomSex
      age <- randomAge
      body <- randomBody
      head <-  randomHead
    } yield chars.model.Human(sex, age, body, head)
}
