package AOC2021

import AOC2020.Parser

import scala.annotation.tailrec

case class BingoNumber(value: Int, isMarked: Boolean) {
  override def toString: String = s"($value, $isMarked)"
}

case class RawBoard(numbers: Vector[Vector[BingoNumber]])

case class Board(numbers: Vector[Vector[BingoNumber]], index: Int) {

  def getBingoNumber(row: Int, column: Int): BingoNumber = numbers(row)(column)

  override def toString: String = numbers.mkString("\n")

  def markNumber(number: Int): Board = {
    val markedBoard = numbers.map { row =>
      row.map { bingoNumber =>
        if (bingoNumber.value == number) {
          BingoNumber(bingoNumber.value, true)
        } else {
          BingoNumber(bingoNumber.value, bingoNumber.isMarked)
        }
      }
    }
    Board(markedBoard, index)
  }

  def sumOfAllUnmarkedNumbers: Int = numbers.flatten.foldLeft(0){ (acc, num) => if (!num.isMarked){ acc + num.value} else acc }

  def numberOfMarkedNumbers: Int = numbers.flatten.count(b => b.isMarked)

  def findMarkedRowOrColumn(grid: Vector[Vector[BingoNumber]], boardIndex: Int): Option[(Int, Int)] = {
    val fullyMarkedRows = grid.zipWithIndex.map{ cols => (cols._1.forall(_.isMarked), cols._2)}
    val markedRowsWithBoardNum = fullyMarkedRows.filter(row => row._1 == true).toList
    markedRowsWithBoardNum match {
      case Nil => None
      case head :: Nil => Some((head._2, boardIndex))
      case head :: _ => Some((head._2, boardIndex))
    }
  }

  /** returns (index of marked row, index of board) if exists */
  def checkFullyMarkedRow: Option[(Int, Int)] = {
    findMarkedRowOrColumn(numbers, index)
  }

  /** returns (index of marked column, index of board) if exists */
  def checkFullyMarkedColumn: Option[(Int, Int)] = {
    findMarkedRowOrColumn(numbers.transpose, index)
  }

  /** returns the index of the board if it has a bingo*/
  def checkBingo: Option[Int] = {
    (checkFullyMarkedRow, checkFullyMarkedColumn) match {
      case (Some(winningRow), None) => Some(index)
      case (None, Some(winningColumn)) => Some(index)
      case (Some(winningRow), Some(winningColumn)) => Some(index)
      case (None, None) => None
      case _ => None
    }
  }

}

object Day4Puzzle extends App {

  val parser = new Parser[RawBoard]("day4.txt")
  def parseRawBoard(boardString: String): RawBoard = {
    val rows = boardString.split("\n").toVector
    val split = rows.map(rowString => rowString.split(" ").toVector)
    val parsed = split.map(board => board.filterNot(s => s == "").map(stringNum => BingoNumber(stringNum.toInt, false)))
    RawBoard(parsed)
  }

  val numbersDrawn = List(1,76,38,96,62,41,27,33,4,2,94,15,89,25,66,14,30,0,71,21,48,44,87,73,60,50,77,45,29,18,5,99,65,16,93,95,37,3,52,32,46,80,98,63,92,24,35,55,12,81,51,17,70,78,61,91,54,8,72,40,74,68,75,67,39,64,10,53,9,31,6,7,47,42,90,20,19,36,22,43,58,28,79,86,57,49,83,84,97,11,85,26,69,23,59,82,88,34,56,1)
  //val numbersDrawn = List(7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1)
  val boards: List[Board] = parser.parseMultiline(parseRawBoard).zipWithIndex.map(rawBoard => Board(rawBoard._1.numbers, rawBoard._2))


  @tailrec
  def findWinningBoardPart1(currentBoards: List[Board], numbersDrawn: List[Int]): Int = {
    if (numbersDrawn.nonEmpty){
      val currentNumber = numbersDrawn.head
      val markCurrent = currentBoards.map(board => board.markNumber(currentNumber))

      val checkBingo = markCurrent.map(board => board.checkBingo) //check whether any board has won
      val winningBoard = checkBingo.flatten

      winningBoard match {
        case winningBoard :: _ => markCurrent(winningBoard).sumOfAllUnmarkedNumbers * currentNumber
        case Nil => findWinningBoardPart1(markCurrent, numbersDrawn.tail)
      }
    } else {
      0
    }
  }

  val score = findWinningBoardPart1(boards, numbersDrawn)

  //println("Solution part 1: " + score)

  @tailrec
  def findWinningBoardPart2(currentBoards: List[Board], numbersDrawn: List[Int], winningBoardsWithCalledNumbers: List[(Board, Int)]): Int = {
    if (numbersDrawn.nonEmpty && currentBoards.nonEmpty){

      println()
      val currentNumber = numbersDrawn.head
      println("numbers to process: " + numbersDrawn)
      println(s"processed boards and their winning numbers: ${winningBoardsWithCalledNumbers.map(_._2)}")
      val markCurrent = currentBoards.map(board => board.markNumber(currentNumber))

      val checkBingo = markCurrent.map(board => board.checkBingo) //check whether any board has won
      println("Length of markCurrent: " + (markCurrent.length))
      val winningBoards = checkBingo.flatten
      println("Length of boards not won yet: " + (markCurrent.length - winningBoards.length))
      println("Winning boards: " + winningBoards)

      winningBoards match {
        case winningBoardIndexes @ _ :: _  => {
          val removeWinningBoards = markCurrent.zipWithIndex.filterNot(elem => winningBoardIndexes.toSet.contains(elem._2))
          println("boards left: " + removeWinningBoards.map(_._1).length)
          val winningBoards = markCurrent.zipWithIndex.filter(elem => winningBoardIndexes.toSet.contains(elem._2))
          val currentWinningBoardsWithCalledNumber: List[(Board, Int)] = winningBoards.map(i => (i._1, currentNumber))

          findWinningBoardPart2(removeWinningBoards.map(_._1).zipWithIndex.map(e => Board(e._1.numbers, e._2)), numbersDrawn.tail, winningBoardsWithCalledNumbers ::: currentWinningBoardsWithCalledNumber)
        }
        case Nil => findWinningBoardPart2(markCurrent, numbersDrawn.tail, winningBoardsWithCalledNumbers)
      }
    } else {
      println(s"score calculation: ${winningBoardsWithCalledNumbers.last._1.sumOfAllUnmarkedNumbers} * ${winningBoardsWithCalledNumbers.last._2}")
      winningBoardsWithCalledNumbers.last._1.sumOfAllUnmarkedNumbers * winningBoardsWithCalledNumbers.last._2
    }
  }

  val scorePart2 = findWinningBoardPart2(boards, numbersDrawn, List.empty)

  println("Solution part 2: " + scorePart2)

}
