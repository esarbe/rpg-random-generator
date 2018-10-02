package chars.model

import chars.model.human.{Age, Head}

case class Human(
  sex: Sex,
  age: Age,
  body: Body,
  head: Head
)