package chars.text

trait Description
object Description {
  case class Node(label: String, descriptions: Seq[Description]) extends Description
  case class Leaf(label: String, description: String) extends Description
}

trait DescriptionBuilder[T] {
  def describe(t: T): Description
}

object DescriptionBuilders {

  def buildDescription[T: DescriptionBuilder](t: T): Description = {
    implicitly[DescriptionBuilder[T]].describe(t)
  }
}
