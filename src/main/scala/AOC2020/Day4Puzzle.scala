package AOC2020

import AOC2020.Day3Puzzle.parseRow

case class Passport(
  byr: Boolean,
  iyr: Boolean,
  eyr: Boolean,
  hgt: Boolean,
  hcl: Boolean,
  ecl: Boolean,
  pid: Boolean,
  cid: Boolean
)

case object Passport {
  val byrCode = "byr"
  val iyrCode = "iyr"
  val eyrCode = "eyr"
  val hgtCode = "hgt"
  val hclCode = "hcl"
  val eclCode = "ecl"
  val pidCode = "pid"
  val cidCode = "cid"
}

object Day4Puzzle extends App {

  def parseRow(line: String): Passport = {
    Passport(
      line.contains(Passport.byrCode),
      line.contains(Passport.iyrCode),
      line.contains(Passport.eyrCode),
      line.contains(Passport.hgtCode),
      line.contains(Passport.hclCode),
      line.contains(Passport.eclCode),
      line.contains(Passport.pidCode),
      line.contains(Passport.cidCode)
    )
  }

  val parser = new Parser[Passport]("day4-test.txt", parseRow)
  val pp = parser.parseMultiLine("\n")

  pp.foreach(println)

  parser.close()

}
