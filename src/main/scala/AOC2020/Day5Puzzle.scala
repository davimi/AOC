package AOC2020

import scala.annotation.tailrec


sealed trait RowIndicator
case object F extends RowIndicator {
  override def toString: String = "F"
}
case object B extends RowIndicator {
  override def toString: String = "B"
}


sealed trait SeatIndicator
case object L extends SeatIndicator {
  override def toString: String = "L"
}
case object R extends SeatIndicator {
  override def toString: String = "R"
}



case class Boardingpass(rowSeq: Seq[RowIndicator], seatSeq: Seq[SeatIndicator]) {

  def calcSeat(): Seat = {
    val row = traverseRows(rowSeq, 0, 127)
    val seat = traverseSeats(seatSeq, 0, 7)
    Seat(row, seat)
  }

  @tailrec
  private def traverseRows(rowSeq: Seq[RowIndicator], rowMin: Int, rowMax: Int): Int = {
    /*println()
    println(rowSeq.map(_.toString))
    println(s"rowMin: $rowMin, rowMax: $rowMax")*/

    if (rowSeq.length == 1) {
      rowSeq.head match {
        case F => rowMin
        case B => rowMax
      }
    } else {
      val dist = calcDist(rowMin, rowMax)
      rowSeq.head match {
        case F => traverseRows(rowSeq.tail, rowMin, rowMax - dist)
        case B => traverseRows(rowSeq.tail, rowMin + dist, rowMax)
      }
    }
  }

  @tailrec
  private def traverseSeats(seatSeq: Seq[SeatIndicator], seatMin: Int, seatMax: Int): Int = {
    if (seatSeq.length == 1) {
      seatSeq.head match {
        case L => seatMin
        case R => seatMax
      }
    } else {
      val dist = calcDist(seatMin, seatMax)
      seatSeq.head match {
        case L => traverseSeats(seatSeq.tail, seatMin, seatMax - dist)
        case R => traverseSeats(seatSeq.tail, seatMin + dist, seatMax)
      }
    }
  }

  def calcDist(min: Int, max: Int): Int = {
    val isEven = (max - min) % 2 == 0
    if (isEven) {
      (max - min) / 2
    } else {
      ((max - min) / 2) + 1
    }
  }
}

case class Seat(row: Int, col: Int) {

  val seatId: Int = (row * 8) + col

}


object Day5Puzzle extends App {

  def parseLine(line: String): Boardingpass = {
    val rowSeq = line.slice(0, 7).map {
      case 'F' => F
      case 'B' => B
      case _ => throw new Exception("Unexpected Input")
    }

    val seatSeq = line.slice(7, 10).map {
      case 'L' => L
      case 'R' => R
      case _ => throw new Exception("Unexpected Input")
    }
    Boardingpass(rowSeq, seatSeq)
  }

  val parser = new Parser[Boardingpass]("day5.txt", parseLine)
  val boardingpasses = parser.parse()
  parser.close()

  val seats = boardingpasses.map(_.calcSeat()).sortBy(_.row)

  println("Max seat id (part 1): " + seats.map(_.seatId).max)

  val seatIds = seats.map(_.seatId).sorted

  val range: Set[Int] = (seatIds.min to seatIds.max).toSet

  println("Your seat (part 2): " + (range &~ seatIds.toSet).head)

}
