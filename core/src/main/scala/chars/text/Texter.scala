package chars.text

trait Description
object Description {
  case class Node(label: String, descriptions: Seq[Description]) extends Description
  case class Leaf(label: String, description: String) extends Description
}

trait DescriptionBuilder[T] {
  def describe(t: T): Description
}


object DescriptionBuilder {

  implicit class DescriptionBuilderOps[T](t: T)(implicit ev: DescriptionBuilder[T]) {
    def describe: Description = ev.describe(t)
  }
}
