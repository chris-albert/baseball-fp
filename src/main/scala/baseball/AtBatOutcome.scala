package baseball

import cats.data._
import cats.data.Validated._
import cats.implicits._
import Parser.InvalidAtBatOutcome
import baseball.OutType.StrikeOut

trait AtBatOutcome

object AtBatOutcome {

  case class Hit(
    hitType: HitType,
    toPosition: Position,
    trajectory: HitTrajectory,
    desc: Option[String]
  ) extends AtBatOutcome

  case class Out(
    outType: OutType,
    positions: Seq[Position] = Seq(),
    outs: Int = 1,
    bunt: Boolean = false,
    sacrifice: Boolean = false,
    desc: Option[String] = None
  ) extends AtBatOutcome

  case object HitByPitch extends AtBatOutcome
  case object Walk extends AtBatOutcome

  val hitRegEx = raw"^(Single|Double|Triple|Home Run) to (\w+) \(([\w\s]+)\);?([\w\s]*)".r
  val outRegEx = raw"^(Flyball|Foul Flyball|Foul Popfly|Popfly|Ground Ball Double Play|Groundout|Lineout): ([\w-]+)/?([\w\s]*);? ?(.*)"r

  def parse(desc: String): Parser.Result[AtBatOutcome] = {
    (desc match {
      case hitRegEx(hitType, toPosition, trajectory, rest) =>
//        println(s"i got here at least [$hitType] [$toPosition] [$trajectory]")
        for {
          ht <- HitType.parse(hitType)
          tp <- Position.parse(toPosition)
          t  <- HitTrajectory.parse(trajectory)
        } yield Hit(ht,tp,t,emptyAsNone(rest.trim)).valid
      case outRegEx(outType, positions, forceOrSac, rest) =>
//        println(s"[$outType][$positions][$forceOrSac][$rest]")
        for {
          ot <- OutType.parse(outType)
          p  <- Position.parseMany(positions)
        } yield Out(
          outType = ot,
          positions = p,
          outs = OutType.outs(outType),
          bunt = OutType.isBunt(outType),
          sacrifice = OutType.isSacrifice(forceOrSac),
          desc = emptyAsNone(rest)
        ).valid
      case "Walk"               => Some(Walk.valid)
      case "Hit By Pitch"       => Some(HitByPitch.valid)
      case "Strikeout Looking"  => Some(Out(StrikeOut(false)).valid)
      case "Strikeout Swinging" => Some(Out(StrikeOut(true)).valid)
      case _ => None
    }).getOrElse(InvalidAtBatOutcome(desc).invalidNel)
  }

  def emptyAsNone(s: String): Option[String] =
    if(s.isEmpty) None else Some(s)
}

trait Base

object Base {

  case object First extends Base
  case object Second extends Base
  case object Third extends Base
  case object Home extends Base

  def parse(base: String): Option[Base] = base match {
    case "1B" => Some(First)
    case "2B" => Some(Second)
    case "3B" => Some(Third)

  }

}

trait HitType

object HitType {

  case object Single extends HitType
  case object Double extends HitType
  case object Triple extends HitType
  case object HomeRun extends HitType

  def parse(hit: String): Option[HitType] = hit match {
    case "Single"   => Some(Single)
    case "Double"   => Some(Double)
    case "Triple"   => Some(Triple)
    case "Home Run" => Some(HomeRun)
    case _ => None
  }
}

trait OutType

object OutType {

  case class FlyBall(fair: Boolean = true) extends OutType
  case class PopFly(fair: Boolean = true) extends OutType
  case object GroundOut extends OutType
  case object LineOut extends OutType
  case class StrikeOut(swinging: Boolean) extends OutType

  def parse(out: String): Option[OutType] = out match {
    case "Flyball"                 => Some(FlyBall())
    case "Foul Flyball"            => Some(FlyBall(false))
    case "Popfly"                  => Some(PopFly())
    case "Foul Popfly"             => Some(PopFly(false))
    case "Bunt Groundout"          => Some(GroundOut)
    case "Lineout"                 => Some(LineOut)
    case "Ground Ball Double Play" =>  Some(GroundOut)
    case _ => None
  }

  def isSacrifice(sac: String): Boolean = sac.startsWith("Sacrifice")
  def isBunt(bunt: String): Boolean = bunt.startsWith("Bunt")
  def outs(out: String): Int =
    if(out.endsWith("Double Play")) 2
    else if(out.endsWith("Triple Play")) 3
    else 1
}

trait HitTrajectory

object HitTrajectory {

  case object GroundBall extends HitTrajectory
  case object LineDrive extends HitTrajectory
  case object FlyBall extends HitTrajectory

  def parse(trajectory: String): Option[HitTrajectory] = trajectory match {
    case "Ground Ball" => Some(GroundBall)
    case "Line Drive"  => Some(LineDrive)
    case "Fly Ball"    => Some(FlyBall)
    case _ => None
  }
}

trait Position

object Position {

  case object Pitcher extends Position
  case object Catcher extends Position
  case object FirstBase extends Position
  case object SecondBase extends Position
  case object ThirdBase extends Position
  case object ShortStop extends Position
  case object LeftField extends Position
  case object CenterField extends Position
  case object RightField extends Position

  def parse(pos: String): Option[Position] = pos match {
    case "P"  => Some(Pitcher)
    case "C"  => Some(Catcher)
    case "1B" => Some(FirstBase)
    case "2B" => Some(SecondBase)
    case "3B" => Some(ThirdBase)
    case "SS" => Some(ShortStop)
    case "LF" => Some(LeftField)
    case "CF" => Some(CenterField)
    case "RF" => Some(RightField)
    case _    => None
  }

  def parseMany(pos: String): Option[List[Position]] =
    pos
      .replace(" unassisted","")
      .split("-")
      .map(parse)
      .toList
      .sequence
}