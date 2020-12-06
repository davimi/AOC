package AOC2020


sealed trait PassportValidation {
  val byrCode = "byr"
  val iyrCode = "iyr"
  val eyrCode = "eyr"
  val hgtCode = "hgt"
  val hclCode = "hcl"
  val eclCode = "ecl"
  val pidCode = "pid"
  val cidCode = "cid"
}

case class PassportValidationPart1(
                     byrPresent: Boolean,
                     iyrPresent: Boolean,
                     eyrPresent: Boolean,
                     hgtPresent: Boolean,
                     hclPresent: Boolean,
                     eclPresent: Boolean,
                     pidPresent: Boolean,
                     cidPresent: Boolean
) {
  val requiredFieldsArePresent: Boolean = byrPresent && iyrPresent && eyrPresent && hgtPresent && hclPresent && eclPresent && pidPresent
}

case object PassportValidationPart1 extends PassportValidation


case class PassportValidationPart2(
                                  byr: Option[String],
                                  iyr: Option[String],
                                  eyr: Option[String],
                                  hgt: Option[String],
                                  hcl: Option[String],
                                  ecl: Option[String],
                                  pid: Option[String]
                                  ) {

  val allValid = byrIsValid && iyrIsValid && eyrIsValid && hgtIsValid && hclIsValid && eclIsValid && pidIsValid

  def byrIsValid: Boolean = {
    byr match {
      case None => false
      case Some(s) => (1920 <= s.toInt) && (s.toInt <= 2002)
    }
  }

  def iyrIsValid: Boolean = {
    iyr match {
      case None => false
      case Some(s) => (2010 <= s.toInt) && (s.toInt <= 2020)
    }
  }

  def eyrIsValid: Boolean = {
    eyr match {
      case None => false
      case Some(s) => (2020 <= s.toInt) && (s.toInt <= 2030)
    }
  }

  def hgtIsValid: Boolean = {
    hgt match {
      case None => false
      case Some(s) => s.takeRight(2) match {
        case "cm" => (s.dropRight(2).toInt >= 150) && (s.dropRight(2).toInt <= 193)
        case "in" => (s.dropRight(2).toInt >= 59) && (s.dropRight(2).toInt <= 76)
        case _ => false
      }
    }
  }

  def hclIsValid: Boolean = {
    hcl match {
      case None => false
      case Some(_) => true
    }
  }

  def eclIsValid: Boolean = {
    ecl match {
      case None => false
      case Some(s) => (s.length == 3) && (s.contains("amb") || s.contains("blu") || s.contains("brn") || s.contains("gry") || s.contains("grn") || s.contains("hzl") || s.contains("oth"))
    }
  }

  def pidIsValid: Boolean = {
    pid match {
      case None => false
      case Some(s) => s.length == 9
    }
  }

}

case object PassportValidationPart2 extends PassportValidation


object Day4Puzzle extends App {

  def parsePassportPart1(str: String): PassportValidationPart1 = {
    val concat = str.split("\n").toList.fold("")(_ + " " + _)
    PassportValidationPart1(
      concat.contains(PassportValidationPart1.byrCode),
      concat.contains(PassportValidationPart1.iyrCode),
      concat.contains(PassportValidationPart1.eyrCode),
      concat.contains(PassportValidationPart1.hgtCode),
      concat.contains(PassportValidationPart1.hclCode),
      concat.contains(PassportValidationPart1.eclCode),
      concat.contains(PassportValidationPart1.pidCode),
      concat.contains(PassportValidationPart1.cidCode)
    )
  }

  def parsePassportPart2(str: String): PassportValidationPart2 = {
    val concat = str.split("\n").toList.fold("")(_ + " " + _)
    PassportValidationPart2(
      """byr:(\d{0,4})""".r.findFirstMatchIn(concat).map(m => m.group(1)),
      """iyr:(\d{0,4})""".r.findFirstMatchIn(concat).map(m => m.group(1)),
      """eyr:(\d{0,4})""".r.findFirstMatchIn(concat).map(m => m.group(1)),
      """hgt:(\d*(?:cm)?(?:in)?)""".r.findFirstMatchIn(concat).map(m => m.group(1)),
      """hcl:(#(?:[0-9]|[a-f]){6})""".r.findFirstMatchIn(concat).map(m => m.group(1)),
      """ecl:(\w{3})""".r.findFirstMatchIn(concat).map(m => m.group(1)),
      """pid:(\d*)""".r.findFirstMatchIn(concat).map(m => m.group(1)),
    )
  }

  val parser = new Parser[PassportValidationPart1]("day4.txt")
  val passportsPart1 = parser.parseMultiline(parsePassportPart1)

  println("Answer part1: " + passportsPart1.count(_.requiredFieldsArePresent))

  val parserPart2 = new Parser[PassportValidationPart2]("day4.txt")
  val passportsPart2 = parserPart2.parseMultiline(parsePassportPart2)

/*
  passportsPart2.foreach(println)
  passportsPart2.foreach(i => println("byr : " + i.byrIsValid))
  passportsPart2.foreach(i => println("iyr : " + i.iyrIsValid))
  passportsPart2.foreach(i => println("eyr : " + i.eyrIsValid))
  passportsPart2.foreach(i => println("hgt : " + i.hgtIsValid))
  passportsPart2.foreach(i => println("hcl : " + i.hclIsValid))
  passportsPart2.foreach(i => println("ecl : " + i.eclIsValid))
  passportsPart2.foreach(i => println("pid : " + i.pidIsValid))
*/

  println("Answer part2: " + passportsPart2.count(_.allValid))

  parser.close()

}
