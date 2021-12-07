package AOC2021

import scala.annotation.tailrec
import scala.collection.immutable.ListMap

object Day6Puzzle extends App {

  case class Lanternfish(daysUntilSpawnsNewFish: Int) {

    def spawnNewFish(): Lanternfish = Lanternfish(8)

    def resetDaysUntilSpawnsNewFish(): Lanternfish = Lanternfish(6)
  }

  val data = List(2,4,1,5,1,3,1,1,5,2,2,5,4,2,1,2,5,3,2,4,1,3,5,3,1,3,1,3,5,4,1,1,1,1,5,1,2,5,5,5,2,3,4,1,1,1,2,1,4,1,3,2,1,4,3,1,4,1,5,4,5,1,4,1,2,2,3,1,1,1,2,5,1,1,1,2,1,1,2,2,1,4,3,3,1,1,1,2,1,2,5,4,1,4,3,1,5,5,1,3,1,5,1,5,2,4,5,1,2,1,1,5,4,1,1,4,5,3,1,4,5,1,3,2,2,1,1,1,4,5,2,2,5,1,4,5,2,1,1,5,3,1,1,1,3,1,2,3,3,1,4,3,1,2,3,1,4,2,1,2,5,4,2,5,4,1,1,2,1,2,4,3,3,1,1,5,1,1,1,1,1,3,1,4,1,4,1,2,3,5,1,2,5,4,5,4,1,3,1,4,3,1,2,2,2,1,5,1,1,1,3,2,1,3,5,2,1,1,4,4,3,5,3,5,1,4,3,1,3,5,1,3,4,1,2,5,2,1,5,4,3,4,1,3,3,5,1,1,3,5,3,3,4,3,5,5,1,4,1,1,3,5,5,1,5,4,4,1,3,1,1,1,1,3,2,1,2,3,1,5,1,1,1,4,3,1,1,1,1,1,1,1,1,1,2,1,1,2,5,3)

  val fish: List[Lanternfish] = data.map(i => Lanternfish(i))

  @tailrec
  def runSimulation(population: List[Lanternfish], numDays: Int = 80): Int = {
    if (numDays == 0) population.length
    else {
      val (parents, notParents) = population.partition(f => f.daysUntilSpawnsNewFish == 0)
      val newBorns: List[Lanternfish] = parents.map(_.spawnNewFish())
      val parentsUpdate: List[Lanternfish] = parents.map(_.resetDaysUntilSpawnsNewFish())
      val updateNotParents: List[Lanternfish]= notParents.map(f => Lanternfish(f.daysUntilSpawnsNewFish - 1))

      runSimulation(updateNotParents ::: parentsUpdate ::: newBorns, numDays - 1)
    }
  }

  val solutionPart1 = runSimulation(fish)
  println("Solution part 1: " + solutionPart1)

  /**
   *
   * @param population Map (number of days until fish makes a baby) -> number of those fish
   * @return number of total fish at end of simulation
   */
  def runSimulationStackEfficient(population: Map[Int, BigInt], numDays: Int = 256): BigInt = {
    if (numDays == 0) population.values.sum
    else {
      val numParents = population.getOrElse(0, BigInt(0))
      val notParents = population.filterKeys(_ != 0)
      val numNewborns: BigInt = numParents

      val updateNotParentsMap: Map[Int, BigInt] = notParents.keys.foldLeft(Map(): Map[Int, BigInt])((acc, key) => acc.updated(key - 1, notParents.getOrElse(key, 0)))

      val newPopulation: Map[Int, BigInt] = updateNotParentsMap.updated(8, numNewborns).updated(6, updateNotParentsMap(6) + numParents)

      runSimulationStackEfficient(newPopulation, numDays - 1)
    }
  }

  val initialFishMap = fish.map(_.daysUntilSpawnsNewFish).foldLeft(Map(0 -> 0, 1 -> 0, 2 -> 0, 3 -> 0, 4 -> 0, 5 -> 0, 6 -> 0, 7 -> 0, 8 -> 0))((acc, num) => acc.updated(num, acc(num) + 1)).mapValues(BigInt(_))

  val solutionPart2 = runSimulationStackEfficient(initialFishMap, 256)

  println("Solution part 2: " + solutionPart2)

}
