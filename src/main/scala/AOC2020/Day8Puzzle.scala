package AOC2020

sealed trait Instruction {
  def value: Int
  def numExec: Int

  def updateNumExec(): Unit

  def exec(): Unit
}
case class Jump(value: Int, var numExec: Int) extends Instruction {
  override def exec(): Unit = ()

  override def updateNumExec() = numExec += 1
}
case class Acc(value: Int, var numExec: Int) extends Instruction {
  override def exec(): Unit = {
    AccumulatorPart1.value += value
  }

  override def updateNumExec() = numExec += 1
}
case class NoOperation(value: Int, var numExec: Int) extends Instruction {
  override def exec(): Unit = ()

  override def updateNumExec() = numExec += 1
}

case object AccumulatorPart1 {
  var value: Int = 0
}

case class InstructionHistory(history: Vector[Instruction])

case class InstructionSheet(var instructions: Vector[Instruction]) {

  var currentInstruction: Int = 0

  def part1() = {
    do {
      instructions(currentInstruction) match {
        case Jump(v, _) =>
          currentInstruction += v
          instructions(currentInstruction).updateNumExec()
        case other =>
          other.exec()
          instructions(currentInstruction).updateNumExec()
          currentInstruction += 1
      }
    } while (instructions(currentInstruction).numExec < 2)
  }

  def part2() = {
    //brute force?
    var instructionHistory = InstructionHistory(Vector.empty)
    do {
      instructions(currentInstruction) match {
        case Jump(v, _) =>
          instructionHistory.history :+ instructions(currentInstruction)
          currentInstruction += v
          instructions(currentInstruction).updateNumExec()
        case other =>
          other.exec()
          instructionHistory.history :+ instructions(currentInstruction)
          instructions(currentInstruction).updateNumExec()
          currentInstruction += 1
      }
    } while (currentInstruction < instructions.length)
  }
}


object Day8Puzzle extends App {

  def parseInstructions(line: String): Instruction = {
    line.take(3) match {
      case "jmp" => Jump(parseNum(line), 0)
      case "acc" => Acc(parseNum(line), 0)
      case "nop" => NoOperation(parseNum(line), 0)
      case s => throw new Exception("unexpected input: " + s)
    }
  }

  def parseNum(s: String): Int = {
    val regex = """(?:acc|jmp|nop) ((?:\+|-)\d+)""".r
    val num = regex.findFirstMatchIn(s).map(_.group(1)).get
    num match {
      case s if s.head == '+' => s.tail.toInt
      case s if s.head == '-' => s.tail.toInt * -1
      case s => throw new Exception("unexpected input number: " + s)
    }
  }

  val parser = new Parser[Instruction]("day8.txt")
  val instructionSheetPart1 = InstructionSheet(parser.parse(parseInstructions).toVector)

  instructionSheetPart1.part1()

  println("acc value (part1): " + AccumulatorPart1.value)

  val parser2 = new Parser[Instruction]("day8.txt")
  val instructionSheetPart2 = InstructionSheet(parser2.parse(parseInstructions).toVector)

  instructionSheetPart2.instructions.foreach(println)


}
