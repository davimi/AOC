package object AOC2021 {

  def prettyPrintFormat[A](m: Seq[Seq[A]]): String = {
    m.transpose.map(_.mkString(" ")).mkString("\n")
  }

}
