package baseball

import java.nio.file.Paths
import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.scaladsl._
import akka.stream._
import akka.util.ByteString

object BaseballReference extends App {

  implicit val actorSystem = ActorSystem()
  implicit val materializer = ActorMaterializer()
  implicit val executionContext = actorSystem.dispatcher

  val resourcePath = "src/main/resources/baseball-reference/KC-SF-2014-10-29.csv"
  def start() = {
    println("Hi baseball")
    FileIO.fromPath(Paths.get(resourcePath))
      .via(Framing.delimiter(ByteString("\n"), 2048, true).map(_.utf8String))
      .filter(_.matches("^[bt][0-9].*"))
      .runForeach(println)
      .map{_ =>
        println("Stream complete")
        actorSystem.terminate()
      }
      .recover{case e =>
        println("Stream Error", e)
        actorSystem.terminate()
      }
  }
  start()

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
    first: Option[Int],
    second: Option[Int],
    third: Option[Int]
  )

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

}