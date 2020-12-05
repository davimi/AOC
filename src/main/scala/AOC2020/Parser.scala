package AOC2020
import scala.annotation.tailrec
import scala.io.Source
import scala.util.Try


class Parser[A](fileName: String, parseLine: String => A) {

  private val file = Try(Source.fromResource("AOC2020/" + fileName))
  private val lines =  file.map(_.getLines.toList).get

  def parseSingleLine(): List[A] = {
    lines.map(parseLine)
  }

  def parseMultiLine(entitySeparator: String): List[A] = {
    recurseMultiLine(lines, List.empty, entitySeparator)
  }

  @tailrec
  private def recurseMultiLine(remainingLines: Seq[String], acc: List[A], entitySeparator: String ): List[A] = {
    //println(remainingLines)
    if (remainingLines.isEmpty) {
      acc
    } else {
      val entityString = remainingLines.takeWhile(line => line.length != 0)

      val newRemaining = if (remainingLines.length == 1) {
        remainingLines
      } else if (remainingLines.length > 1){
        remainingLines.dropWhile(line => line.length > 0).tail
      }
      else List.empty
      println(newRemaining.length)

      //println(s"entityString: $entityString")

      val entityStringSingleLine = entityString.fold("")((x, y) => x + " " + y)
      val entity = parseLine(entityStringSingleLine)
      recurseMultiLine(newRemaining, acc :+ entity, entitySeparator)
    }
  }

  def close() = file.map(_.close())
}
