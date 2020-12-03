package AOC2020
import scala.io.Source
import scala.util.Try


class Parser[A](fileName: String, parseLine: String => A) {

  private val file = Try(Source.fromResource("AOC2020/" + fileName))
  private val lines =  file.map(_.getLines.toList).get

  def parse(): List[A] = {
    lines.map(parseLine)
  }

  def close() = file.map(_.close())
}
