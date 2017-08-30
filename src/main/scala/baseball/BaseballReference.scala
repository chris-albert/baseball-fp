package baseball

object BaseballReference {

  def parse(fileName: String): String = {
    ???
  }

  case class BaseballReferenceLine(inning: Inning,
                                   score: Score,
                                   out: Int,
                                   runnersOnBase: RunnersOnBase,
                                   pitches: Int,
                                   pitchCount: PitchCount,
                                   pitchSequence: PitchSequence)

  case class Inning(`type`: Inning.Type,
                    number: Int)

  object Inning {
    object Type extends Enumeration {
      val Top    = Value("t")
      val Bottom = Value("b")
    }

    type Type = Type.Value
  }

  case class Score(home: Int,
                   away: Int)

  case class RunnersOnBase(first: Option[Int],
                           second: Option[Int],
                           third: Option[Int])

  case class PitchCount(balls: Int, strikes: Int)

  case class PitchSequence(s: Seq[String])

}
