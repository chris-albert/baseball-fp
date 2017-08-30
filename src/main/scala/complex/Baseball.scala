package complex

object Baseball {

  import Things._

  sealed trait Event

  case class PlayerOnDefense(player: Player, position: Things.Position) extends Event
  case class PlayerActive(player: Player, position: Things.Position, replaced: Option[Player] = None)
  case class PlayerToPlate(player: Player) extends Event
  case class PitchToPlayer(pitch: Things.Pitch,mph: Int) extends Event
  case class PitchOutcome(result: Things.PitchResult) extends Event
  case class PlayerOut(seq: List[Things.Position],out: Things.Out) extends Event
  case class PlayerMovesBase(base: Things.Base) extends Event
  case object StartInning extends Event
  case object EndInning extends Event

  case class Player(name: String, number: Int, team: Team)

  case class AtBat(hitter: Player, pitcher: Player, pitches: List[Event]) extends Event


  case class BoxScore(score: Int, hits: Int, errors: Int)

  case class GameBoxScore(home: BoxScore, away: BoxScore)


  def main(args: Array[String]) = {
    val _ = ""
  }
}

object Things {

  sealed trait Team
  case object HomeTeam extends Team
  case object AwayTeam extends Team

  sealed trait InFielder
  sealed trait OutFielder

  class Position(val num: Int)
  case object Pitcher extends Position(1) with InFielder
  case object Catcher extends Position(2) with InFielder
  case object FirstBase extends Position(3) with InFielder
  case object SecondBase extends Position(4) with InFielder
  case object ThirdBase extends Position(5) with InFielder
  case object ShortStop extends Position(6) with InFielder
  case object LeftField extends Position(7) with OutFielder
  case object CenterField extends Position(8) with OutFielder
  case object RightField extends Position(9) with OutFielder
  case object DesignatedHitter extends Position(0)

  val positions: List[Position] = List(
    Pitcher,
    Catcher,
    FirstBase,
    SecondBase,
    ThirdBase,
    ShortStop,
    LeftField,
    CenterField,
    RightField
  )

  sealed trait Out
  case object FlyOut extends Out
  case object LineOut extends Out
  case object FoulTip extends Out
  case object PopOut extends Out
  case object GroundOut extends Out
  case object GroundOutDoublePlay extends Out
  case object InFieldFly extends Out

  sealed trait Pitch
  case object FastBall    extends Pitch
  case object CurveBall   extends Pitch
  case object Slider      extends Pitch
  case object KnuckleBall extends Pitch
  case object ChangeUp extends Pitch
  case object Sinker extends Pitch
  case object Cutter extends Pitch

  sealed trait Hit
  case object Single  extends Hit
  case object Double  extends Hit
  case object Triple  extends Hit
  case object HomeRun extends Hit

  sealed trait PitchResult
  case object Ball extends PitchResult
  case object SwingingStrike extends PitchResult
  case object CalledStrike extends PitchResult
  case object Foul extends PitchResult
  case object InPlay extends PitchResult
  case object Out extends PitchResult
  case object Struck extends PitchResult

  sealed trait Base
  case object First extends Base
  case object Second extends Base
  case object Third extends Base
  case object Home extends Base
}
