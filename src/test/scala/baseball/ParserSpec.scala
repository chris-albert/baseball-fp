package baseball

import baseball.BaseballReference.{BaseballReferenceLine, Inning, PitchCount, PitchSequence, Pitches, RunnersOnBase, RunsOuts, Score}
import baseball.Parser._
import cats.data.Validated.Valid
import org.scalatest.{Matchers, WordSpec}
import cats.data._
import cats.data.Validated._
import cats.implicits._

class ParserSpec extends WordSpec with Matchers {

  "parse" should {
    "return invalid line" in {
      Parser.parseLine("").isInvalid shouldBe true
    }
//    "return valid line" in {
//      Parser.parseLine(
//        "t1,0-0,0,---,5(1-2) CSBFX,O,SFG,Gregor Blanco,Jeremy Guthrie,-2%,48%,Flyball: CF"
//      ) shouldBe Valid(BaseballReferenceLine(
//        inning = Inning(Inning.Type.Top, 1),
//        score = Score(0,0),
//        out = 0,
//        runnersOnBase = RunnersOnBase(None, None, None),
//        pitches = Pitches(5, PitchCount(1,2), PitchSequence(Seq("C","S","B","F","X"))),
//        runsOuts = RunsOuts(0,1),
//        atBat = "SFG",
//        batter = "Gregor Blanco",
//        pitcher = "Jeremy Guthrie",
//        winningTeamsWinProbability = -2,
//        winningTeamsWinExpectancy = 48,
//        description = "Flyball: CF"
//      ))
//    }
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
    }
    "be valid" in {
      Parser.parseRunnersOnBase("---") shouldBe RunnersOnBase(None, None, None).valid
      Parser.parseRunnersOnBase("1-3") shouldBe RunnersOnBase(Some(1), None, Some(3)).valid
    }
  }
}
