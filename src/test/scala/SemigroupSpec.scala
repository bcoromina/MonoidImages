
import org.scalatest._

class SemigroupSpec extends org.scalatest.funspec.FixtureAnyFunSpec{

  it("associativity for composable images semigroups"){ _ =>
    val a = FileImageUtils.resourceAsComposableImage("multilineWithLabelTest.png")
    val b = FileImageUtils.resourceAsComposableImage("pointsNoLabel.png")
    val c = FileImageUtils.resourceAsComposableImage("poligonsEtiFromJson.png")

    implicit val s = MonoidInstances.insComposableImage
    SemigroupLaws.associativity(a,b,c, ImageUtils.visualCompare(99))
  }



  override protected def withFixture(test: OneArgTest): Outcome =
    test(new FixtureParam)

  class FixtureParam {

  }
}



object SemigroupLaws{
  //combine(x, combine(y, z)) = combine(combine(x, y), z)
  def associativity[T](a: T, b: T, c: T, eq: (T,T) => Boolean)
                      (implicit semigroup: Semigroup[T])= {
    eq(
      semigroup.combine(a,
        semigroup.combine(b,c)
      ),
      semigroup.combine(
        semigroup.combine(a,b),
        c
      )
    )

  }

  def identities[T](a:T,eq: (T,T) => Boolean)(implicit monoid: Monoid[T]) = {
    val aid = monoid.combine(a, monoid.empty)
    assert(eq(aid, a), "Right identity not accomplished")

    val ida =  monoid.combine(monoid.empty, a)
    assert(eq(ida , a), "Left identity not accomplished")
  }
}