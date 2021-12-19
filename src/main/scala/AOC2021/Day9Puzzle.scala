package AOC2021

import scala.collection.mutable.{Map => MutableMap}
import AOC2020.Parser
import AOC2021.Day9Puzzle.Part1.computedHeightMap

import scala.annotation.tailrec

case class Floorpoint(value: Int, isLowPoint: Boolean, row: Int, col: Int, riskLevel: Option[Int], isBasinCounted: Boolean)

object Floorpoint {

  def apply(value: Int, isLowPoint: Boolean, row: Int, col: Int): Floorpoint = {
    if (isLowPoint) {
      Floorpoint(value, isLowPoint = true, row, col, Some(value + 1), false)
    } else {
      Floorpoint(value, isLowPoint = false, row, col, None, false)
    }
  }

}
case class HeightMap(points: Seq[Seq[Floorpoint]]) {

  def apply(r: Int)(c: Int): Floorpoint = points(r)(c)

  def getNeighborsSave(row: Int, col: Int): List[Floorpoint] = {
    val maxRow = points.length - 1
    val maxCol = points.head.length - 1

    val top = if (row == 0) List.empty else List(this(row - 1)(col))
    val right = if (col == maxCol) List.empty else List(this(row)(col + 1))
    val down = if (row == maxRow) List.empty else List(this(row + 1)(col))
    val left = if (col == 0) List.empty else List(this(row)(col - 1))

    top ::: right ::: down ::: left
  }

  def determineLowPoint(row: Int, col: Int): Boolean = {
    getNeighborsSave(row, col).forall(f => f.value > this(row)(col).value)
  }

  def computeLowPoints(): HeightMap = {
    HeightMap(
      this.points.map(row => row.map{ f =>
        if (this.determineLowPoint(f.row, f.col)) {
          //println("Low point found")
          Floorpoint(f.value, true, f.row, f.col)
        } else f
      })
    )

  }

  def computeOverAllRisk(): Int = {
    this.points.flatMap(r => r.filter(_.isLowPoint)).flatMap(_.riskLevel).sum
  }

}

object Day9Puzzle extends App {

  val parser: Parser[Seq[Int]] = Parser[Seq[Int]]("day9.txt")
  def parseLavaTubeLine(line: String): Seq[Int] = line.map(_.toString.toInt)
  val rawData: Seq[Seq[Int]] = parser.parse(parseLavaTubeLine)

  object Part1 {

    val heightMap = HeightMap(rawData.zipWithIndex.map(row => row._1.zipWithIndex.map(d => Floorpoint(d._1, isLowPoint = false, row._2, d._2))))

    val computedHeightMap: HeightMap = heightMap.computeLowPoints()

    println("Solution part 1: " + computedHeightMap.computeOverAllRisk())

  }


  object Part2 {

    val lowPoints: Seq[Floorpoint] = Part1.computedHeightMap.points.flatten.filter(_.isLowPoint)

    @tailrec
    def computeBasinSize(lowPoint: Floorpoint, neighborsToExplore: List[Floorpoint], countedNeighbors: Set[Floorpoint], count: Int): Int = {

      if (neighborsToExplore.isEmpty) count
      else {
        val currentNeighbor = neighborsToExplore.head

        val newNeighbors = Part1.heightMap.getNeighborsSave(currentNeighbor.row, currentNeighbor.col)
          .filter(n => n.value != 9)
          .filter(n => !countedNeighbors.contains(currentNeighbor))

        val newCount = if (countedNeighbors.contains(currentNeighbor)) count else count + 1

        computeBasinSize(lowPoint, neighborsToExplore.tail ::: newNeighbors, countedNeighbors + currentNeighbor, newCount) //BFS
      }

    }

    val basins = lowPoints.map{fp =>
      val initialNeighbors = Part1.heightMap.getNeighborsSave(fp.row, fp.col)
        .filter(_.value != 9)
      computeBasinSize(fp, initialNeighbors, Set(fp), 1) //start with count=1 because the lowpoint itslef is a basin
    }

    //println("Basins: " + basins.sorted.reverse.mkString(", "))
    println("Solution part 2: " + basins.sorted.reverse.slice(0, 3).product)

  }

  Part1

  Part2

}
