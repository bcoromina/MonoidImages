import org.scalatest.Outcome

class MonoidSpec extends org.scalatest.funspec.FixtureAnyFunSpec{
  it("identity"){ _ =>
    val a = FileImageUtils.resourceAsComposableImage("poligonsEtiFromJson.png")
    implicit val s = MonoidInstances.insComposableImage

    SemigroupLaws.identities(a, ImageUtils.visualCompare(99))
  }

  override protected def withFixture(test: OneArgTest): Outcome =
    test(new FixtureParam)

  class FixtureParam {

  }
}
