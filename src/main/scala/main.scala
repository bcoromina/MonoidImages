import scalaphash.PHash.{radialHash, radialHashDistance}

import java.awt.image.BufferedImage
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import javax.imageio.ImageIO
import scala.collection.parallel.CollectionConverters.IterableIsParallelizable


object MoniodSandBox {
  def main(args: Array[String]): Unit = {
    import MonoidSyntax._
    val imageA = new ComposableImage(Array.empty)
    val imageB = new ComposableImage(Array.empty)

    implicit val l = MonoidInstances.insComposableImage
    List(imageA, imageB).combine()

    println("Hello, world!")
  }
}

//combine(x, combine(y, z)) = combine(combine(x, y), z)
trait Semigroup[A] {
  def combine(a: A, b: A): A
}

/* leftIdentity, right identity
combine(x, id) = combine(id, x) = x
 */

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid{
  def apply[A](implicit a: Monoid[A]): Monoid[A] = a

  def combineAll[A :Monoid](elements: Seq[A]): A =
      elements.par.foldLeft(Monoid[A].empty)(Monoid[A].combine)

}

class ComposableImage(val bytes: Array[Byte]){
  val height = ImageIO.read(new ByteArrayInputStream(bytes)).getHeight
  val width = ImageIO.read(new ByteArrayInputStream(bytes)).getWidth
}

object ImageUtils{
  def visualCompare( percentage: Double = 99.99)(img1: ComposableImage, img2: ComposableImage): Boolean = {
    val imageSimilarity = for{
      img1 <- radialHash(ImageIO.read(new ByteArrayInputStream(img1.bytes)))
      img2 <- radialHash(ImageIO.read(new ByteArrayInputStream(img2.bytes)))
    }yield{
      radialHashDistance(img1,img2)
    }

    imageSimilarity match{
      case Right(d) if(d > percentage/100 )=>
        true
      case _ =>
        false
    }

  }

}

object ComposableImage {

  //https://index.scala-lang.org/poslegm/scala-phash/scala-phash/1.2.2?target=_2.13
  implicit class ComparableComposableImage(img1: ComposableImage) {
    def visualEq(img2: ComposableImage, percentage: Float = 99): Boolean = ImageUtils.visualCompare(percentage)(img1, img2 )

  }

}

object MonoidInstances{



  val insComposableImage: Monoid[ComposableImage] = new Monoid[ComposableImage]{
    override def empty: ComposableImage = {
      val resultImage = new BufferedImage(
        1,
        1,
        BufferedImage.TYPE_INT_ARGB )
      /*
      val imageBounds =
        new Rectangle(0, 0, 1, 1)

      val gr: Graphics2D = resultImage.createGraphics
      val c = new Color(255, 255, 255, 1)
      gr.setPaint(c)
      gr.fill(imageBounds)
*/
      val output = new ByteArrayOutputStream()
      ImageIO.write(resultImage, "PNG", output)
      new ComposableImage(output.toByteArray)
    }

    override def combine(a: ComposableImage, b: ComposableImage): ComposableImage = {

      val targetHeight = scala.math.max(a.height,b.height)
      val tagetWidth = scala.math.max(a.width,b.width)

      val resultImage = new BufferedImage(
        tagetWidth,
        targetHeight,
        BufferedImage.TYPE_INT_ARGB )


      val graphics = resultImage.getGraphics
      List(a,b).foreach{ img =>
        if(img.bytes.length > 0){
          val originalImage = ImageIO.read( new ByteArrayInputStream(img.bytes))
          //val scaledImage = originalImage.getScaledInstance(img.width, img.height, Image.SCALE_DEFAULT)
          graphics.drawImage(originalImage, 0,0,null)
        }
      }
      val output = new ByteArrayOutputStream()
      ImageIO.write(resultImage, "PNG", output)

      new ComposableImage(output.toByteArray)
    }
  }


}

object MonoidSyntax{


  implicit class MonoidOps[A: Monoid](elements: Seq[A]){
    def combine() = Monoid.combineAll(elements)
  }


}