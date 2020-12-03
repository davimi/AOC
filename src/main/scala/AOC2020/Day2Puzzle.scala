package AOC2020

import scala.util.matching.Regex

case class PasswordReq(min: Int, max: Int, letter: String, password: String)

object Day2Puzzle extends App {

  def getInputData = {
    val regex = """(\d\d?)-(\d\d?) ([a-z]): (.+)""".r

    def parseLine(line: String): PasswordReq = {
      val pwr = regex.findFirstMatchIn(line).map { g =>
        PasswordReq(g.group(1).toInt, g.group(2).toInt, g.group(3), g.group(4))
      }
      pwr match {
        case Some(x) => x
        case None => throw new Exception("No match")
      }
    }

    val parser = new Parser[PasswordReq]("day2.txt", parseLine)
    parser.parse()
  }

  val inputData = getInputData


  val numcorrectPwsPart1 = inputData.map { pwr =>
    val numOccurences = pwr.password.count(_ == pwr.letter.head)
    (numOccurences >= pwr.min) && (numOccurences <= pwr.max)
  }.count(_ == true)

  println(s"Numer of correct pws (part 1): $numcorrectPwsPart1")


  val numcorrectPwsPart2 = inputData.map { pwr =>
    val position1 = pwr.password.charAt(pwr.min - 1)
    val position2 = pwr.password.charAt(pwr.max - 1)
    val cond1 = ((position1 == pwr.letter.head) && (position2 != pwr.letter.head)) || ((position1 != pwr.letter.head) && (position2 == pwr.letter.head))
    val cond2 = !((position1 == pwr.letter.head) && (position2 == pwr.letter.head))

    (cond1 && cond2)
  }.count(_ == true)

  println(s"Numer of correct pws (part 2): $numcorrectPwsPart2")

}
