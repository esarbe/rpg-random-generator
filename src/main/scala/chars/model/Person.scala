package chars.model

import io.rbricks.itemized.annotation.indexedEnum

object Person {

  @indexedEnum trait Color {
    type Index = Int
    object Red { 0 }
    object Blue { 1 }
    object Yellow { 2 }
  }

  @indexedEnum trait Sex {
    type Index = Int
    object Male { 0 }
    object Female { 1 }
  }

  @indexedEnum trait Age {
    type Index = Int
    object Young { 0 }
    object Middleaged { 1 }
    object Old { 2 }
  }

  @indexedEnum trait CategorizedValue {
    type Index = Int
    object Small { 0 }
    object Medium { 1 }
    object Large { 2 }
  }

  case class Height(value: CategorizedValue)
  case class Weight(value: CategorizedValue)

  object Face {
    @indexedEnum trait Shape {
      type Index = Int
      object Round { 0 }
      object Oval { 1 }
    }
  }

  case class Person(sex: Sex, age: Age, height: Height, weight: Weight)

}
