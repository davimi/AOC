package AOC2020

object Day9Puzzle extends App {

  def checkPreamble(preamble: List[Long], numberToCheck: Long) = {
   val check = for {
     first <- preamble
     second <- preamble if first + second == numberToCheck
   } yield (first + second)

    check.nonEmpty
  }

  val parser = new Parser[Long]("day9.txt")
  val inputData = parser.parse(line => line.toLong)

  val preambleLength = 25
  val check = inputData.sliding(preambleLength + 1).flatMap { xmas =>
    val numberToCheck = xmas.last
    xmas.map {_ => (checkPreamble(xmas, numberToCheck), numberToCheck) }
  }

  val wrongNumber = check.filter(i => !i._1).toList.head._2
  println("first wrong number (part 1): " + wrongNumber)

  var notDone = true
  var listLength = 2
  do {
    inputData.sliding(listLength).foreach { l =>
      if (l.sum == wrongNumber) {
        notDone = false
        println(s"part2: ${l.min} + ${l.max} = ${l.min + l.max}")
      }
    }
    listLength += 1
  } while (notDone)

}
