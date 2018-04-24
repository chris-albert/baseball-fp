package baseball

import baseball.BaseballReference.{BaseballReferenceLine, Inning, PitchCount, PitchSequence, Pitches, RunnersOnBase, RunsOuts, Score}
import cats.data._
import cats.data.Validated._
import cats.implicits._
import scala.util.{Success, Try}

trait Parser {

  type Result[A] = ValidatedNel[ParseValidation,A]

  def parseLine(line: String): Result[BaseballReferenceLine] =
    parseColumns(line) andThen { seq =>
      (
        //TODO: I hate this
        parseInning(seq(0)),
        parseScore(seq(1)),
        parseOuts(seq(2)),
        parseRunnersOnBase(seq(3)),
        parsePitches(seq(4)),
        parseRunsOuts(seq(5)),
        parseAtBat(seq(6)),
        parseBatter(seq(7)),
        parsePitcher(seq(8)),
        parseWTWP(seq(9)),
        parseWTWE(seq(10)),
        parseDescription(seq(11))
      ).mapN(BaseballReferenceLine)
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

  def parseRunnersOnBase(rob: String): Result[RunnersOnBase] =
    (for {
      first  <- isIndexChar(rob, 0, '1', Set('1','-'))
      second <- isIndexChar(rob, 1, '2', Set('2','-'))
      third  <- isIndexChar(rob, 2, '3', Set('3','-'))
    } yield RunnersOnBase(first, second, third)) match {
      case None => InvalidRunnersOnBase(rob).invalidNel
      case Some(runnersOn) => runnersOn.valid
    }

  private def isIndexChar(s: String, index: Int, char: Char, validChars: Set[Char]): Option[Boolean] =
    if(s.length > index && validChars.contains(s(index))) {
      Some(s(index) == char)
    } else {
      None
    }

  val pitchesRegEx = raw"^([0-9].*)\(([0-3])\-([0-2])\) (.*)".r

  def parsePitches(pitches: String): Result[Pitches] =
    pitches match {
      case pitchesRegEx(number, balls, strikes, sequence) =>
        Pitches(
          number = number.toInt,
          count = PitchCount(balls.toInt, strikes.toInt),
          sequence = PitchSequence(sequence.split(""))
        ).valid
      case _ => InvalidPitches(pitches).invalidNel
    }

  val runsOutsRegEx = raw"(O*)(R*)".r

  def parseRunsOuts(ro: String): Result[RunsOuts] =
    ro.sorted match {
      case runsOutsRegEx(outs, runs) =>
        RunsOuts(runs.length, outs.length).valid
      case _ => InvalidRunsOuts(ro).invalidNel
    }

  def parseAtBat(ab: String): Result[String] =
    if(ab.isEmpty) {
      InvalidAtBat(ab).invalidNel
    } else {
      ab.valid
    }

  def parseBatter(batter: String): Result[String] =
    if(batter.isEmpty) {
      InvalidBatter(batter).invalidNel
    } else {
      batter.valid
    }

  def parsePitcher(pitcher: String): Result[String] =
    if(pitcher.isEmpty) {
      InvalidPitcher(pitcher).invalidNel
    } else {
      pitcher.valid
    }

  val percentageRegEx = raw"(-?[0-9]+)%".r

  def parseWTWP(wtwp: String): Result[Int] =
    wtwp match {
      case percentageRegEx(p) => p.toInt.valid
      case _ => InvalidWinningTeamsWinProbability(wtwp).invalidNel
    }

  def parseWTWE(wtwe: String): Result[Int] =
    wtwe match {
      case percentageRegEx(p) => p.toInt.valid
      case _ => InvalidWinningTeamsWinExpectancy(wtwe).invalidNel
    }

//  val descriptionRegEx = raw"(^[\w\s]*):".r

  def parseDescription(desc: String): Result[String] =
    if(desc.isEmpty) {
     InvalidDescription(desc).invalidNel
    } else {
      desc.valid
    }

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

  case class InvalidAtBat(ab: String) extends ParseValidation {
    override def message: String = s"Invalid at bat [$ab]"
  }

  case class InvalidBatter(batter: String) extends ParseValidation {
    override def message: String = s"Invalid batter [$batter]"
  }

  case class InvalidPitcher(pitcher: String) extends ParseValidation {
    override def message: String = s"Invalid pitcher [$pitcher]"
  }

  case class InvalidWinningTeamsWinProbability(wtwp: String) extends ParseValidation {
    override def message: String = s"Invalid winning teams win probability [$wtwp]"
  }

  case class InvalidWinningTeamsWinExpectancy(wtwe: String) extends ParseValidation {
    override def message: String = s"Invalid winning teams win expectancy [$wtwe]"
  }

  case class InvalidDescription(desc: String) extends ParseValidation {
    override def message: String = s"Invalid description [$desc]"
  }
}

object Parser extends Parser
