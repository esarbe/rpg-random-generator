package chars.app.text

import cats.Show
import chars.model._
import chars.model.human._
import chars.text._
import enumeratum.EnumEntry


object HumanDescriptionBuilder extends DescriptionBuilder[Human] {

  import Description._
  import DescriptionBuilder._

  implicit def enumShow[T <: EnumEntry]: Show[T] = _.entryName.toLowerCase

  def eye(
    implicit
    cd: DescriptionBuilder[face.eye.Color],
    sd: DescriptionBuilder[face.eye.Shape],
  ): String => DescriptionBuilder[face.Eye] = {
    label =>
      eye =>
        val descriptions =
          Seq(
            eye.color.describe,
            eye.shape.describe
          )

        Node(label, descriptions)
  }


  def eyes(
    ed: String => DescriptionBuilder[face.Eye]
  ): DescriptionBuilder[face.Eyes] = {
    case face.Eyes.SameEyes(eye) =>
      ed("eyes").describe(eye)
    case face.Eyes.DifferentEyes(left, right) =>
      Node("eyes",
        Seq(
          ed("right").describe(right),
          ed("left").describe(left)
        )
      )
  }


  def faceDescriptionBuilder(
    implicit
    fd: DescriptionBuilder[face.Shape],
    ed: DescriptionBuilder[face.Eyes]
  ): DescriptionBuilder[Face] = { face =>

    val descriptions =
      Seq(
        face.shape.describe,
        face.eyes.describe
      )

    Node(face.getClass.getSimpleName, descriptions)
  }



  def describeBody(
    implicit
    hd: DescriptionBuilder[body.Height],
    wd: DescriptionBuilder[body.Weight],
    ad: DescriptionBuilder[body.Athleticism],
  ): DescriptionBuilder[Body] = { body=>

    val descriptions =
      Seq(
        body.height.describe,
        body.weight.describe,
        body.athleticism.describe
      )

    Node("body", descriptions)
  }

  def humanoidDescriptionBuilder(
    implicit
    ad: DescriptionBuilder[Age],
    sd: DescriptionBuilder[Sex],
    bd: DescriptionBuilder[Body],
    hd: DescriptionBuilder[Head]
  ): DescriptionBuilder[Human] = { human =>
    import human._

    val descriptions =
      Seq(
        age.describe,
        sex.describe,
        body.describe,
        head.describe,
      )

    Node("human", descriptions)
  }



  val sd = leaf[Sex]("sex")
  val ad = leaf[Age]("age", (_: Age).value.toString)
  val bhd = leaf("height", (_: body.Height).value.entryName)
  val bwd = leaf[body.Weight]("weight", (_: body.Weight).value.entryName)
  val bad = leaf[body.Athleticism]("athleticism", (_: body.Athleticism).value.entryName)
  val hbd = describeBody(bhd, bwd, bad)
  val fed =
    eye(
      leaf[face.eye.Color]("color"),
      leaf[face.eye.Shape]("shape")
    )
  val fesd = eyes(fed)
  val hfsd = leaf[face.Shape]("shape")
  val hfd = faceDescriptionBuilder(hfsd, fesd)
  val hfs = leaf[CategorizedValue]("size")
  val hhd = headDescriptionBuilder(hfd, hfs)

  def leaf[T: Show](label: String): DescriptionBuilder[T] =
    t => Leaf(label, implicitly[Show[T]].show(t))

  def leaf[T](label: String, f: T => String): DescriptionBuilder[T] =
    t => Leaf(label, f(t))

  def headDescriptionBuilder(
    implicit
    fd: DescriptionBuilder[human.Face],
    sd: DescriptionBuilder[CategorizedValue],
  ): DescriptionBuilder[Head] = { head =>
    val descriptions =
      Seq(
        head.size.describe,
        head.face.describe,
      )

    Node("head", descriptions)
  }

  def humanDescriptionBuilder(
    ad: DescriptionBuilder[Age],
    sd: DescriptionBuilder[Sex],
    hd: DescriptionBuilder[Head],
    bd: DescriptionBuilder[Body]
  ): DescriptionBuilder[Human] = { h: Human =>
      val descriptions =
        Seq(
          ad.describe(h.age),
          sd.describe(h.sex),
          hd.describe(h.head),
          bd.describe(h.body)
        )

      Node("human", descriptions)
  }

  def describe(h: Human): Description = humanoidDescriptionBuilder(ad, sd, hbd,  hhd).describe(h)
}
