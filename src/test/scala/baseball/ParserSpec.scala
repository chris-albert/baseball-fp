package baseball

import baseball.AtBatOutcome._
import baseball.BaseballReference._
import baseball.HitTrajectory.{GroundBall, LineDrive}
import baseball.HitType._
import baseball.OutType._
import baseball.Parser._
import baseball.Position.{CenterField, RightField, SecondBase}
import cats.data.Validated.Valid
import cats.implicits._
import org.scalatest.{Matchers, WordSpec}

class ParserSpec extends WordSpec with Matchers {

  "parse" should {
    "return invalid line" in {
      Parser.parseLine("").isInvalid shouldBe true
    }
    "return valid line" in {
//      Parser.parseLine(
//        "t1,0-0,0,---,5(1-2) CSBFX,O,SFG,Gregor Blanco,Jeremy Guthrie,-2%,48%,Flyball: CF"
//      ) shouldBe Valid(BaseballReferenceLine(
//        inning = Inning(Inning.Type.Top, 1),
//        score = Score(0,0),
//        out = 0,
//        runnersOnBase = RunnersOnBase.empty,
//        pitches = Pitches(5, PitchCount(1,2), PitchSequence(Seq("C","S","B","F","X"))),
//        runsOuts = RunsOuts(0,1),
//        atBat = "SFG",
//        batter = "Gregor Blanco",
//        pitcher = "Jeremy Guthrie",
//        winningTeamsWinProbability = -2,
//        winningTeamsWinExpectancy = 48,
//        description = "Flyball: CF"
//      ))
    }
  }

  "parseInning" should {
    "be invalid if not proper format" in {
      Parser.parseInning("") shouldBe InvalidInning("").invalidNel
    }
    "be valid if bottom 1st" in {
      Parser.parseInning("b1") shouldBe Inning(Inning.Type.Bottom,1).valid
      Parser.parseInning("t11") shouldBe Inning(Inning.Type.Top,11).valid
    }
  }

  "parseScore" should {
    "be invalid if not proper format" in {
      Parser.parseScore("") shouldBe InvalidScore("").invalidNel
    }
    "be valid score" in {
      Parser.parseScore("0-0") shouldBe Score(0,0).valid
      Parser.parseScore("1-10") shouldBe Score(1,10).valid
    }
  }

  "parseOuts" should {
    "be invalid if not proper format" in {
      Parser.parseOuts("a") shouldBe InvalidOuts("a").invalidNel
      Parser.parseOuts("4") shouldBe InvalidOuts("4").invalidNel
    }
    "be valid outs" in {
      Parser.parseOuts("0") shouldBe 0.valid
      Parser.parseOuts("3") shouldBe 3.valid
    }
  }

  "parseRunnersOnBase" should {
    "be invalid if not proper format" in {
      Parser.parseRunnersOnBase("a") shouldBe InvalidRunnersOnBase("a").invalidNel
      Parser.parseRunnersOnBase("12a") shouldBe InvalidRunnersOnBase("12a").invalidNel
      Parser.parseRunnersOnBase("321") shouldBe InvalidRunnersOnBase("321").invalidNel
    }
    "be valid" in {
      Parser.parseRunnersOnBase("---") shouldBe RunnersOnBase(false, false, false).valid
      Parser.parseRunnersOnBase("1-3") shouldBe RunnersOnBase(true, false, true).valid
      Parser.parseRunnersOnBase("123") shouldBe RunnersOnBase(true, true, true).valid
    }
  }

  "parsePitches" should {
    "be invalid if not proper format" in {
      Parser.parsePitches("") shouldBe InvalidPitches("").invalidNel
      Parser.parsePitches("3,(3-3)") shouldBe InvalidPitches("3,(3-3)").invalidNel
      Parser.parsePitches("a,(0-0)") shouldBe InvalidPitches("a,(0-0)").invalidNel
    }
    "be valid" in {
      Parser.parsePitches("2(0-1) H") shouldBe Pitches(2,PitchCount(0,1), PitchSequence(Seq("H"))).valid
      Parser.parsePitches("10(3-0) H") shouldBe Pitches(10,PitchCount(3,0), PitchSequence(Seq("H"))).valid
      Parser.parsePitches("5(1-2) CSBFX") shouldBe Pitches(5,PitchCount(1,2), PitchSequence(Seq("C","S","B","F","X"))).valid
    }
  }

  "parseRunsOuts" should {
    "be invalid if not proper format" in {
      Parser.parseRunsOuts("238") shouldBe InvalidRunsOuts("238").invalidNel
    }
    "be valid" in {
      Parser.parseRunsOuts("") shouldBe RunsOuts(0,0).valid
      Parser.parseRunsOuts("R") shouldBe RunsOuts(1,0).valid
      Parser.parseRunsOuts("O") shouldBe RunsOuts(0,1).valid
      Parser.parseRunsOuts("RRRO") shouldBe RunsOuts(3,1).valid
      Parser.parseRunsOuts("OOR") shouldBe RunsOuts(1,2).valid
      Parser.parseRunsOuts("ROR") shouldBe RunsOuts(2,1).valid
    }
  }

  "parseAtBat" should {
    "be invalid if not proper format" in {
      Parser.parseAtBat("") shouldBe InvalidAtBat("").invalidNel
    }
    "be valid" in {
      Parser.parseAtBat("SFG") shouldBe "SFG".valid
      Parser.parseAtBat("asdf") shouldBe "asdf".valid
    }
  }

  "parseBatter" should {
    "be invalid if not proper format" in {
      Parser.parseBatter("") shouldBe InvalidBatter("").invalidNel
    }
    "be valid" in {
      Parser.parseBatter("Chris Albert") shouldBe "Chris Albert".valid
      Parser.parseBatter("asdf") shouldBe "asdf".valid
    }
  }

  "parsePitcher" should {
    "be invalid if not proper format" in {
      Parser.parsePitcher("") shouldBe InvalidPitcher("").invalidNel
    }
    "be valid" in {
      Parser.parsePitcher("Mad Bum") shouldBe "Mad Bum".valid
      Parser.parsePitcher("asdf") shouldBe "asdf".valid
    }
  }

  "parseWTWP" should {
    "be invalid if not proper format" in {
      Parser.parseWTWP("") shouldBe InvalidWinningTeamsWinProbability("").invalidNel
      Parser.parseWTWP("3") shouldBe InvalidWinningTeamsWinProbability("3").invalidNel
      Parser.parseWTWP("asdf") shouldBe InvalidWinningTeamsWinProbability("asdf").invalidNel
    }
    "be valid" in {
      Parser.parseWTWP("-2%") shouldBe (-2).valid
      Parser.parseWTWP("100%") shouldBe 100.valid
      Parser.parseWTWP("100%") shouldBe 100.valid
    }
  }

  "parseWTWE" should {
    "be invalid if not proper format" in {
      Parser.parseWTWE("") shouldBe InvalidWinningTeamsWinExpectancy("").invalidNel
      Parser.parseWTWE("3") shouldBe InvalidWinningTeamsWinExpectancy("3").invalidNel
      Parser.parseWTWE("asdf") shouldBe InvalidWinningTeamsWinExpectancy("asdf").invalidNel
    }
    "be valid" in {
      Parser.parseWTWE("-2%") shouldBe (-2).valid
      Parser.parseWTWE("100%") shouldBe 100.valid
      Parser.parseWTWE("100%") shouldBe 100.valid
    }
  }

  "parseAtBatOutcome" should {
    "be invalid if not parseable" in {
      Parser.parseAtBatOutcome("") shouldBe InvalidAtBatOutcome("").invalidNel
    }
    "be valid" in {
      Parser.parseAtBatOutcome("Single to 2B (Ground Ball); Sandoval to 2B") shouldBe
        Hit(Single, SecondBase, GroundBall, Some("Sandoval to 2B")).valid

      Parser.parseAtBatOutcome("Single to RF (Line Drive)") shouldBe
        Hit(Single, RightField, LineDrive, None).valid

      Parser.parseAtBatOutcome("Double to RF (Line Drive); Butler Scores") shouldBe
        Hit(HitType.Double, RightField, LineDrive, Some("Butler Scores")).valid

      Parser.parseAtBatOutcome("Strikeout Looking") shouldBe
        Out(StrikeOut(false)).valid

      Parser.parseAtBatOutcome("Strikeout Swinging") shouldBe
        Out(StrikeOut(true)).valid

      Parser.parseAtBatOutcome("Walk") shouldBe Walk.valid

      Parser.parseAtBatOutcome("Hit By Pitch") shouldBe HitByPitch.valid

      Parser.parseAtBatOutcome("Flyball: CF") shouldBe
        Out(FlyBall(), Seq(CenterField)).valid

      Parser.parseAtBatOutcome("Flyball: CF/Sacrifice Fly; Pence Scores") shouldBe
        Out(FlyBall(), Seq(CenterField), sacrifice = true, desc = Some("Pence Scores")).valid
    }
  }

  "parseDescription" should {
    "be invalid if not proper format" in {
      Parser.parseDescription("") shouldBe InvalidDescription("").invalidNel
    }
    "be valid" in {
      Parser.parseDescription("Flyball: CF") shouldBe "Flyball: CF".valid
      Parser.parseDescription("Lineout: RF/Sacrifice Fly; Sandoval Scores; Pence to 3B") shouldBe
        "Lineout: RF/Sacrifice Fly; Sandoval Scores; Pence to 3B".valid
    }
  }

}
