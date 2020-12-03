package AOC2020


sealed trait SlopeObject {
  def symbol: Char
}

case class Tree(position: Int) extends SlopeObject {
  override val symbol: Char = Tree.symbol
}

object Tree {
  val symbol: Char = '#'
}

case class Empty(position: Int) extends SlopeObject {
  override val symbol: Char = Empty.symbol
}

object Empty {
  val symbol: Char = '.'
}

case class SlopeRow(elements: Seq[SlopeObject])

case class Slope(rows: List[SlopeRow]) {
  val patternSize = 31
  val slopeSize = 323
}

object Day3Puzzle extends App {

  def parseRow(line: String): SlopeRow = {
    val x = line.toCharArray.zipWithIndex.map {
      case (Empty.symbol, pos) => Empty(pos)
      case (Tree.symbol, pos) => Tree(pos)
      case (_, _) => throw new Exception("encountered unexpected symbol")
    }
    SlopeRow(x)
  }

  def traverseSlope(slope: Slope, rowStepSize: Int, columnStepSize: Int): Long = {
    var numTrees = 0
    var row = 0
    var col = 0

    for( _ <- 0 until slope.slopeSize by rowStepSize) {
      if (slope.rows(row).elements(col % slope.patternSize).symbol == Tree.symbol) {
          numTrees += 1
        }
      row += rowStepSize
      col += columnStepSize
    }
    numTrees.toLong
  }

  val parser = new Parser[SlopeRow]("day3.txt", parseRow)

  val slope = Slope(parser.parse())

  val part1 = traverseSlope(slope, 1, 3)

  println("Answer part 1: " + part1)

  val part2a = traverseSlope(slope, 1, 1)
  val part2b = traverseSlope(slope, 1, 5)
  val part2c = traverseSlope(slope, 1, 7)
  val part2d = traverseSlope(slope, 2, 1)

  println("Answer part 2: " + {part1 * part2a * part2b * part2c * part2d})


}
