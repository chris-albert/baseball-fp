package baseball

import java.nio.file.Paths
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{FileIO, Framing}
import akka.util.ByteString
import cats.implicits._
import cats.data.Validated.{Invalid, Valid}
import baseball.BaseballReference.{BaseballReferenceLine, Game}
import simple.SimpleBaseball.Box
import simple.SimpleBaseball.Box.BoxColumn

object StreamingParser extends App{

  val resourcePath = "src/main/resources/baseball-reference/KC-SF-2014-10-29.csv"

  def streamFile[A](file: String)(func: Game => A): Unit = {
    implicit val actorSystem = ActorSystem()
    implicit val materializer = ActorMaterializer()
    implicit val executionContext = actorSystem.dispatcher
    FileIO.fromPath(Paths.get(file))
      .via(Framing.delimiter(ByteString("\n"), 2048, true).map(_.utf8String))
      .filter(_.matches("^[bt][0-9].*"))
      .map(Parser.parseLine)
      .runFold(List.empty[Parser.Result[BaseballReferenceLine]]) {
        case (accu, item) => item :: accu
      }
      .map { r =>
        r.sequence match {
          case Valid(res) => func(Game(res.reverse))
          case Invalid(errs) =>
            println(s"Error in parsing stream []")
        }
        println("Stream complete")
        actorSystem.terminate()
      }
      .recover { case e =>
        println("Stream Error", e)
        actorSystem.terminate()
      }
  }

  def start() = {
    streamFile(resourcePath) { result =>

      val box = BaseballReference.getBoxScore(result)
      println(Box.boxAsString(box, BoxColumn("asdfasdf", "SFG", "KCR")))
    }
  }

  start()
}
