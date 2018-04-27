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
    atBatOutCome: AtBatOutcome,
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

}