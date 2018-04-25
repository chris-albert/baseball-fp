package baseball

object Box {

  case class BoxColumn(header: String, away: String, home: String, separator: String = "-") {
    lazy val padd = Seq(header, away, home, separator).map(_.length).max

    def padded(f: BoxColumn => String, p: String): String = paddRight(paddLeft(f(this), padd, p),padd + 1,p)
  }

  case class BoxItem(away: Int, home: Int)

  case class BoxScore(box: List[BoxItem], runs: BoxItem)

  def digits(n: Int) = if (n == 0) 1 else math.log10(math.abs(n)).toInt + 1

  def boxAsString(bs: BoxScore, names: BoxColumn = BoxColumn("", "Away", "Home")): String = {
    val innings = bs.box.zipWithIndex.map { case (boxItem, index) =>
      BoxColumn((index + 1).toString, boxItem.away.toString, boxItem.home.toString)
    }
    val agg = Seq(BoxColumn("R", bs.runs.away.toString, bs.runs.home.toString))
    val columnSep = BoxColumn("|", "|", "|", "-")
    val all = Seq(
      Seq(columnSep),
      Seq(names),
      Seq(columnSep),
      innings,
      Seq(columnSep),
      agg,
      Seq(columnSep)
    ).flatten

    Seq(
      all.map(_.padded(_.header," ")).mkString(""),
      all.map(_.padded(_.separator,"-")).mkString("").init,
      all.map(_.padded(_.away," ")).mkString(""),
      all.map(_.padded(_.home," ")).mkString(""),
      all.map(_.padded(_.separator,"-")).mkString("").init
    ).mkString("\n")
  }

  def fillArr[A](n: Int, a: A): Seq[A] = (0 until n).map(_ => a)

  def fill(n: Int, s: String): String =
    fillArr(n, s).mkString("")

  def padd(s: String, p: String): String = s"$p$s$p"

  def paddRight(s: String, n: Int, p: String): String = {
    val diff = n - s.length
    s + fill(diff, p)
  }

  def paddLeft(s: String, n: Int, p: String): String = {
    val diff = n - s.length
    fill(diff, p) + s
  }
}
