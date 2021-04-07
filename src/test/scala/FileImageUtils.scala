import java.io.FileOutputStream

object FileImageUtils {
  def resourceAsComposableImage(resName:String): ComposableImage = {
    val is = getClass.getResourceAsStream(resName)
    new ComposableImage(is.readAllBytes())
  }

  def writeBytesToFile(bytes: Array[Byte], relativePath: String) = {
    val currentPath = new java.io.File(".").getCanonicalPath
    val os = new FileOutputStream(currentPath + "/" + relativePath)
    os.write(bytes)
    os.close()
  }
}
