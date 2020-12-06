package AOC2020


case class GroupAnswer(numPersons: Int, answerStrings: List[String]) {

  val uniqueAnsers: Set[Char] = answerStrings.foldLeft("")(_ + _).toSet

  val answersPerPerson: List[Set[Char]] = answerStrings.map(i => i.toSet)

  val allYes: Set[Char] = answersPerPerson.foldLeft(answersPerPerson.head)((a, b) => a.intersect(b))

}

object Day6Puzzle extends App {

  def parseLine(str: String): GroupAnswer = {
    val answersOfEachPerson = str.split("\n").toList
    GroupAnswer(answersOfEachPerson.length, answersOfEachPerson)
  }

  val parser = new Parser[GroupAnswer]("day6.txt")
  val groupAnswers = parser.parseMultiline(parseLine)
  parser.close()

  println("Answer part1: " + groupAnswers.map(_.uniqueAnsers.size).sum)

  println("Answer part2: " + groupAnswers.map(_.allYes).map(_.size).sum)



}
