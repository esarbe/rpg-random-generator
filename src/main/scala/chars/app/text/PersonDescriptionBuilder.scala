package chars.app.text

import chars.model.Person
import chars.text.Description.{Leaf, Node}
import chars.text.{Description, DescriptionBuilder}

object PersonDescriptionBuilder extends DescriptionBuilder[Person]{
  override def describe(p: Person): Description = {

    Node("person", Seq(
      Leaf("name", p.name),
      HumanDescriptionBuilder.describe(p.body)
    ))
  }
}
