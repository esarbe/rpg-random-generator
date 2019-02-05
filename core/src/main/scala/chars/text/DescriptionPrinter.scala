package chars.text

import chars.text.Description.{Leaf, Node}

object DescriptionPrinter {

  def print(description: Description): String = print(description, 0)
  def print(description: Description, indentation: Int = 0): String = description match {
    case Leaf(label, value) =>
      "\n" + "\t" * indentation + s"$label: $value"
    case Node(label, descriptions) =>
      "\n" + "\t" * indentation + s"$label:" +
        descriptions.map(print(_, indentation + 1)).mkString
  }

  implicit class DescriptionOps[D: DescriptionBuilder](d: D) {
    def describe: Description = implicitly[DescriptionBuilder[D]].describe(d)
  }
}
