package baseball

import baseball.BaseballReference.{BaseballReferenceLine, Inning, Pitches, RunnersOnBase, RunsOuts, Score}
import cats.data._
import cats.data.Validated._
import cats.implicits._
import scala.util.{Success, Try}

trait Parser {

  type Result[A] = ValidatedNel[ParseValidation,A]

  def parseLine(line: String): Result[BaseballReferenceLine] = {
    parseColumns(line) andThen { seq =>
      (
        parseInning(seq(0)),
        parseScore(seq(1)),
        parseOuts(seq(2)),
        parseRunnersOnBase(seq(3)),
        parsePitches(seq(4)),
        parseRunsOuts(seq(5)),
        parseAtBat(seq(6)),
        parseBatter(seq(7)),
        parsePitchers(seq(8)),
        parseWTWP(seq(9)),
        parseWTWE(seq(10)),
        parseDescription(seq(11))
      ).mapN(BaseballReferenceLine)
    }
  }

  def parseColumns(line: String): Result[Seq[String]] = {
    //TODO this is not correct way to parse csv, need to account for "
    val split = line.split(",")
    if(split.length == 12) split.toSeq.valid else InvalidLine(line).invalidNel
  }

  val inningRegEx = raw"^([bt])([0-9].*)".r

  def parseInning(inning: String): Result[Inning] =
    inning match {
      case inningRegEx(t,n) =>
        Inning(Inning.Type.withName(t),n.toInt).valid
      case _ => InvalidInning(inning).invalidNel
    }

  val scoreRegEx = raw"^([0-9].*)-([0-9].*)".r

  def parseScore(score: String): Result[Score] =
    score match {
      case scoreRegEx(away,home) =>
        Score(away.toInt,home.toInt).valid
      case _ => InvalidScore(score).invalidNel
    }

  def parseOuts(outs: String): Result[Int] =
    Try(outs.toInt) match {
      case Success(v) if v >= 0 && v <= 3 => v.valid
      case _ => InvalidOuts(outs).invalidNel
    }

  def parseRunnersOnBase(rob: String): Result[RunnersOnBase] = {
    InvalidRunnersOnBase(rob).invalidNel
  }

  def parsePitches(pitches: String): Result[Pitches] = ???
  def parseRunsOuts(ro: String): Result[RunsOuts] = ???
  def parseAtBat(ab: String): Result[String] = ???
  def parseBatter(batter: String): Result[String] = ???
  def parsePitchers(pitchers: String): Result[String] = ???
  def parseWTWP(wtwp: String): Result[Int] = ???
  def parseWTWE(wtwe: String): Result[Int] = ???
  def parseDescription(desc: String): Result[String] = ???

  sealed trait ParseValidation {
    def message: String
  }

  case class InvalidLine(line: String) extends ParseValidation {
    override def message: String = s"Unable to parse line [$line]"
  }

  case class InvalidInning(inning: String) extends ParseValidation {
    override def message: String = s"Invalid inning [$inning]"
  }

  case class InvalidScore(score: String) extends ParseValidation {
    override def message: String = s"Invalid score [$score]"
  }

  case class InvalidOuts(outs: String) extends ParseValidation {
    override def message: String = s"Invalid outs [$outs]"
  }

  case class InvalidRunnersOnBase(rob: String) extends ParseValidation {
    override def message: String = s"Invalid runners on base [$rob]"
  }

  case class InvalidPitches(pitches: String) extends ParseValidation {
    override def message: String = s"Invalid pitches [$pitches]"
  }

  case class InvalidRunsOuts(ro: String) extends ParseValidation {
    override def message: String = s"Invalid runs/outs [$ro]"
  }

  case class InvalidWinningTeamsWinProbability(wtwp: String) extends ParseValidation {
    override def message: String = s"Invalid winning teams win probability [$wtwp]"
  }

  case class InvalidWinningTeamsWinExpectancy(wtwe: String) extends ParseValidation {
    override def message: String = s"Invalid winning teams win expectancy [$wtwe]"
  }
}

object Parser extends Parser
