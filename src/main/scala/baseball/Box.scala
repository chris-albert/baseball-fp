package baseball

import baseball.BaseballReference.{BaseballReferenceLine, Game}

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

  case class TeamLines(
    away: List[BaseballReferenceLine],
    home: List[BaseballReferenceLine],
  )

  case class InningLines(
    inningLines: List[BaseballReferenceLine]
  )

  object InningLines {
    def countInnings(il: InningLines): Int = il.inningLines.length
  }

  case class GameWithSplits(
    game: Game,
    teamsLines: TeamLines,
    inningsLines: List[InningLines]
  )

  object GameWithSplits {
    def apply(game: Game): GameWithSplits = {
      val teamSplit = splitAtDiff(game.lines)(_.atBat)
      GameWithSplits(
        game = game,
        teamsLines = TeamLines(
          away = safeAccess(teamSplit,0).getOrElse(List()),
          home = safeAccess(teamSplit,1).getOrElse(List())
        ),
        inningsLines = splitAtDiff(game.lines)(_.inning.number).map(InningLines.apply)
      )
    }
  }

  def safeAccess[A](l: List[A], index: Int): Option[A] =
    if(l.length > index) Some(l(index)) else None

  def splitAtDiff[A,B](list: List[A])(f: A => B): List[List[A]] = {
    def loop(list: List[A], tmpAccu: List[A], accu: List[List[A]], last: Option[B]): List[List[A]] = list match {
      case Nil =>
        tmpAccu :: accu
      case head :: rest if last.contains(f(head)) =>
        //if we are the same as previous
        loop(rest, head :: tmpAccu, accu, Some(f(head)))
      case head :: rest =>
        //if we are diff from previous
        loop(rest, List(head), tmpAccu :: accu, Some(f(head)))
    }
    loop(list, List.empty[A], List.empty[List[A]], list.headOption.map(f)).map(_.reverse).reverse
  }

  def getBoxScore(game: Game): BoxScore = {
    val boxItems: List[BoxItem] =
      splitAtDiff(game.lines)(_.inning.number).flatMap {lineInnings =>
        splitAtDiff(lineInnings)(_.inning.`type`).map {topBottom =>
          topBottom.map(_.runsOuts.runs).sum
        } match {
          case away :: home :: Nil =>
            Some(BoxItem(away, home))
          case away :: Nil =>
            Some(BoxItem(away, 0))
          case _ => None
        }
      }

    BoxScore(boxItems, BoxItem(boxItems.map(_.away).sum,boxItems.map(_.home).sum))
  }

  def getBoxScoreString(game: Game): String = {
    val gameWithSplits = GameWithSplits(game)
    val box = getBoxScore(game)
    val teams = BoxColumn("",
      away = gameWithSplits.teamsLines.away.headOption.map(_.atBat).getOrElse(""),
      home = gameWithSplits.teamsLines.home.headOption.map(_.atBat).getOrElse("")
    )
    Box.boxAsString(box, teams)
  }
}
