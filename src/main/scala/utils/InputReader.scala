package utils
import scala.io.Source
object InputReader {
  def readInput(fileName: String):
    List[String] = {
            val source = 
        Source.fromResource(fileName)
            try source.getLines().toList
            finally source.close()
    }
}
