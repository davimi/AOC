package AOC2020


sealed trait Move
sealed trait Direction extends Move {
    def turnLeft90: Direction
    def turnRight90: Direction
    def turn180: Direction
}

case object North extends Direction {
    override def turnLeft90 = West
    override def turnRight90 = East
    override def turn180: Direction = South
}
case object West extends Direction {
    override def turnLeft90 = South
    override def turnRight90 = North
    override def turn180: Direction = East
}
case object South extends Direction {
    override def turnLeft90 = East
    override def turnRight90 = West
    override def turn180: Direction = North
}
case object East extends Direction {
    override def turnLeft90 = North
    override def turnRight90 = South
    override def turn180: Direction = West
}
case object Left extends Move
case object Right extends Move
case object Forward extends Move


case class ShipInstruction(move: Move, magnitude: Int)

case class Ship(currentDirection: Direction, northDistance: Int, eastDistance: Int, southDistance: Int, westDistance: Int) {


    override def toString: String = {
        s"Ship - facing: $currentDirection; north: $northDistance, west: $westDistance, south $southDistance, east: $eastDistance"
    }

    def getManhattanDistance: Int = {
        math.abs(northDistance - southDistance) + math.abs(eastDistance - westDistance)
    }

    def processInstruction(shipInstruction: ShipInstruction): Ship = {
        println(shipInstruction)
        shipInstruction.move match {
            case Forward => moveShipForward(shipInstruction.magnitude)
            case Left | Right => changeDirection(shipInstruction.move, shipInstruction.magnitude)
            case North => Ship(currentDirection, northDistance + shipInstruction.magnitude, eastDistance, southDistance, westDistance)
            case East => Ship(currentDirection, northDistance, eastDistance + shipInstruction.magnitude, southDistance, westDistance)
            case South => Ship(currentDirection, northDistance, eastDistance, southDistance + shipInstruction.magnitude, westDistance)
            case West => Ship(currentDirection, northDistance, eastDistance, southDistance, westDistance + shipInstruction.magnitude)
        }
    }

    private def moveShipForward(magnitude: Int): Ship = {
        this.currentDirection match {
            case North => Ship(currentDirection, northDistance + magnitude, eastDistance, southDistance, westDistance)
            case West => Ship(currentDirection, northDistance, eastDistance, southDistance, westDistance + magnitude)
            case South => Ship(currentDirection, northDistance, eastDistance, southDistance + magnitude, westDistance)
            case East => Ship(currentDirection, northDistance, eastDistance + magnitude, southDistance, westDistance)
        }
    }

    private def changeDirection(move: Move, magnitude: Int): Ship = {
        (move, magnitude) match {
            case (Left, 90) => Ship(currentDirection.turnLeft90, northDistance, eastDistance, southDistance, westDistance)
            case (Left, 180) => Ship(currentDirection.turn180, northDistance, eastDistance, southDistance, westDistance)
            case (Left, 270) => Ship(currentDirection.turnRight90, northDistance, eastDistance, southDistance, westDistance)
            case (Right, 90) => Ship(currentDirection.turnRight90, northDistance, eastDistance, southDistance, westDistance)
            case (Right, 180) => Ship(currentDirection.turn180, northDistance, eastDistance, southDistance, westDistance)
            case (Right, 270) => Ship(currentDirection.turnLeft90, northDistance, eastDistance, southDistance, westDistance)
            case _ => throw new Exception("Unexpected input")
        }
    }
}

object Day12Puzzle extends App {

    def parseInstruction(str: String) = {
        val move = str.head match {
            case 'N' => North
            case 'W' => West
            case 'S' => South
            case 'E' => East
            case 'L' => Left
            case 'R' => Right
            case 'F' => Forward
            case s => throw new Exception("Unexpected input: " + s)
        }
        ShipInstruction(move, str.tail.toInt)
    }

    val parser = new Parser[ShipInstruction]("day12.txt")
    val instructions = parser.parse(parseInstruction)

    val ship = Ship(East, 0, 0, 0, 0)

    val travelShip = instructions.foldLeft(ship){ (ship, instr) =>
        //println(ship)
        ship.processInstruction(instr)
    }

    println("Manhattan distance ship at the end: " + travelShip.getManhattanDistance)


}
