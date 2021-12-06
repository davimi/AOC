package AOC2021

import AOC2020.Parser

object Day5Puzzle extends App {

  case class StartCoords(x: Int, y: Int)
  case class EndCoords(x: Int, y: Int)
  case class LineInstruction(start: StartCoords, end: EndCoords) {

    override def toString: String = s"(${start.x}, ${start.y}) -> (${end.x}, ${end.y})"

    def isHorizontal: Boolean = (start.x != end.x) && (start.y == end.y)
    def isVertical: Boolean = (start.y != end.y) && (start.x == end.x)
    def isDiagonal: Boolean = !(isHorizontal || isVertical)

    def orderLowToHigh(): LineInstruction = {
      if (isHorizontal) {
        if (start.x <= end.x) this else LineInstruction(StartCoords(end.x, end.y), EndCoords(start.x, start.y))
      } else if (isVertical) {
        if (start.y <= end.y) this else LineInstruction(StartCoords(end.x, end.y), EndCoords(start.x, start.y))
      } else throw new Exception("invalid line instruction")
    }

  }

  def parseLineInstruction(lineStr: String): LineInstruction = {
    val regex = """(\d+),(\d+)\s->\s(\d+),(\d+)""".r
    regex.findFirstMatchIn(lineStr).map { g =>
      LineInstruction(StartCoords(g.group(1).toInt, g.group(2).toInt), EndCoords(g.group(3).toInt, g.group(4).toInt))
    } match {
      case Some(ins) => ins
      case None => throw new Exception("No match")
    }
  }

  val parser = new Parser[LineInstruction]("day5.txt")
  val instructions: List[LineInstruction] = parser.parse(parseLineInstruction)

  val nonDiagonalLines = instructions.filter(ins => ins.isVertical || ins.isHorizontal).map(_.orderLowToHigh())
  val maxX = List(instructions.map(l => l.end.x).max, instructions.map(l => l.start.x).max).max
  val maxY = List(instructions.map(l => l.end.y).max, instructions.map(l => l.start.y).max).max

  val board: Vector[Vector[Int]] = (0 to maxX).map(_ => 0).toVector.map(_ => (0 to maxY).map(_ => 0).toVector) //instructions are inclusive indexes
  println("Size of board: " + board.length, maxY)

  def fillBoardWithInstructions(instructions: List[LineInstruction], board: Vector[Vector[Int]]): Vector[Vector[Int]] = {
    if (instructions.isEmpty) {
      board
    } else {
      val currentInstruction = instructions.head
      //println("Processing instruction: " + currentInstruction)

      val newBoard: Vector[Vector[Int]] = if (currentInstruction.isHorizontal) {
        val markRange = currentInstruction.start.x to currentInstruction.end.x
        markRange.foldLeft(board)((newBoard, x) => {
          newBoard.updated(x,
            newBoard(x).updated(currentInstruction.start.y,
              newBoard(x)(currentInstruction.start.y) + 1))}
        )
      } else if (currentInstruction.isVertical) {
        val markRange = currentInstruction.start.y to currentInstruction.end.y
        markRange.foldLeft(board)((newBoard, y) =>
          newBoard.updated(currentInstruction.start.x, newBoard(currentInstruction.start.x).updated(y, newBoard(currentInstruction.start.x)(y) + 1)))
      } else throw new Exception("invalid instruction for part 1")

      fillBoardWithInstructions(instructions.tail, newBoard)
    }
  }

  val horizontalAndVerticalFilledBoard = fillBoardWithInstructions(nonDiagonalLines, board)

  println("Number of points >= 2 (part 1): " + horizontalAndVerticalFilledBoard.flatten.count(_ >= 2))
  println()

  //----------------


  def fillBoardWithInstructionsDiagonal(instructions: List[LineInstruction], board: Vector[Vector[Int]]): Vector[Vector[Int]] = {
    if (instructions.isEmpty) {
      board
    } else {
      val currentInstruction = instructions.head
      val xdiff = currentInstruction.end.x - currentInstruction.start.x
      val ydiff = currentInstruction.end.y - currentInstruction.start.y

      val positiveXRange = currentInstruction.start.x to currentInstruction.end.x
      val negativeXRange = currentInstruction.start.x to currentInstruction.end.x by -1
      val positiveYRange = currentInstruction.start.y to currentInstruction.end.y
      val negativeYRange = currentInstruction.start.y to currentInstruction.end.y by -1

      val (markRangeX, markRangeY) = (xdiff, ydiff) match {
        case (xd, yd) if xd >= 0 && yd >= 0 => (positiveXRange, positiveYRange)
        case (xd, yd) if xd >= 0 && yd < 0 => (positiveXRange, negativeYRange)
        case (xd, yd) if xd < 0 && yd >= 0 => (negativeXRange, positiveYRange)
        case (xd, yd) if xd < 0 && yd < 0 => (negativeXRange, negativeYRange)
      }
      val newBoard = markRangeX.zip(markRangeY).foldLeft(board)((newBoard, xy) =>
        newBoard.updated(xy._1,
          newBoard(xy._1).updated(xy._2,
            newBoard(xy._1)(xy._2) + 1)))

      fillBoardWithInstructionsDiagonal(instructions.tail, newBoard)
    }
  }

  val diagonalLines = instructions.filterNot(ins => ins.isVertical || ins.isHorizontal)

  val diagonallyFilledBoard = fillBoardWithInstructionsDiagonal(diagonalLines, horizontalAndVerticalFilledBoard)

  //println(diagonallyFilledBoard.transpose.map(_.mkString(" ")).mkString("\n"))

  println("Number of points >= 2 (part 2): " + diagonallyFilledBoard.flatten.count(_ >= 2))

}
