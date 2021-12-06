package AOC2020
import scala.io.{BufferedSource, Source}
import scala.util.Try


class Parser[A](fileName: String) {

  private val file: BufferedSource = Try(Source.fromResource("AOC2021/" + fileName)).get

  def parse(parseLine: String => A): List[A] = {
    val lines = file.getLines.toList
    lines.map(parseLine)
  }

  def parseMultiline(parseMultiline: String => A): List[A] = {
    val entities = file.mkString.split("\\n\\n").toList
    entities.map(parseMultiline)
  }


  def close() = file.close()
}
