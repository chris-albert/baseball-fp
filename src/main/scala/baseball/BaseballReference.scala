package baseball

import baseball.Box._

object BaseballReference {

  case class Game(lines: List[BaseballReferenceLine])

  case class BaseballReferenceLine(
    inning: Inning,
    score: Score,
    out: Int,
    runnersOnBase: RunnersOnBase,
    pitches: Pitches,
    runsOuts: RunsOuts,
    atBat: String,
    batter: String,
    pitcher: String,
    winningTeamsWinProbability: Int,
    winningTeamsWinExpectancy: Int,
    description: String
  )

  case class Inning(`type`: Inning.Type,
                    number: Int)

  object Inning {
    object Type extends Enumeration {
      val Top    = Value("t")
      val Bottom = Value("b")
    }

    type Type = Type.Value
  }

  case class Score(
    first: Int,
    second: Int
  )

  case class RunnersOnBase(
    first: Boolean,
    second: Boolean,
    third: Boolean
  )

  object RunnersOnBase {
    def empty: RunnersOnBase = RunnersOnBase(
      first = false,
      second = false,
      third = false
    )
  }

  case class Pitches(
    number: Int,
    count: PitchCount,
    sequence: PitchSequence
  )

  case class PitchCount(
    balls: Int,
    strikes: Int
  )

  case class PitchSequence(
    sequence: Seq[String]
  )

  case class RunsOuts(
    runs: Int,
    outs: Int
  )

  object RunsOuts {
    def empty: RunsOuts = RunsOuts(
      runs = 0,
      outs = 0
    )
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