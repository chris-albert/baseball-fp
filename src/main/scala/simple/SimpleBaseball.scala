package simple

import baseball.Box
import baseball.Box.{BoxItem, BoxScore}

object SimpleBaseball {
  def main(args: Array[String]) = {
    println("Simple Baseball event source")
    process
  }

  def process = {
    val commands = List(
      //1st
      Out, Out, Out,
      Run, Out, Out, Out,
      //2nd
      Run, Out, Run, Out, Out,
      Out, Out, Out,
      //3rd
      Out, Out, Out,
      Out, Out, Out,
      //4th
      Out, Out, Out,
      Out, Out, Out,
      //5Th
      Out, Out, Out
    )

    val teamAgg = processCommands(commands)
    printList(teamAgg)
    val box = buildBoxScore(teamAgg)
    println(Box.boxAsString(box))
  }

  def processCommands[A](cs: List[Command]): List[GameAggregate] = {
    cs.foldLeft(GameAggregate() -> List[GameAggregate]()) { case ((agg, accu), cmd) =>
      val na = agg.apply(agg.process(cmd))
      na -> (na :: accu)
    }._2
  }

  def printList[A](l: List[A]): Unit = {
    l.foreach(println)
  }

  def buildBoxScore(ls: List[GameAggregate]): BoxScore = {
    val gameFlattened = ls.reverse.map { game =>
      (game.isAwayBatting, game.outs, game.inning, game.away.runs, game.home.runs)
    }
    val groupedByInnings = gameFlattened.groupBy(_._3)
    val box = groupedByInnings.map { inning =>
      val groupedByTeam = inning._2.groupBy(_._1)
      inning._1 -> BoxItem(
        groupedByTeam.get(true).fold(0)(s => s.map(_._4).last - s.map(_._4).head),
        groupedByTeam.get(false).fold(0)(s => s.map(_._5).last - s.map(_._5).head)
      )
    }.toSeq.sortBy(_._1).map(_._2)
    val lastInning = ls.last
    BoxScore(box.toList, BoxItem(lastInning.away.runs, lastInning.home.runs))
  }


  def asdf: Unit = {
    val Ticketfly = (1, 2, (a: String) => a)
    val (scala, music, live_events) = Ticketfly
  }

  sealed trait Command

  case object Run extends Command

  case object Out extends Command

  sealed trait Event

  case object RunEvent extends Event

  case object OutEvent extends Event

  trait Aggregate[A] {

    def processCmd(cmd: Command): Event = process.apply(cmd)

    def process: PartialFunction[Command, Event]

    def applyEvent(e: Event): A = apply.apply(e)

    def apply: PartialFunction[Event, A]
  }

  case class GameAggregate(away: TeamAggregate = TeamAggregate(),
                           home: TeamAggregate = TeamAggregate()) extends Aggregate[GameAggregate] {

    val outs          = away.outs + home.outs
    val isHomeBatting = outs % 6 >= 3
    val isAwayBatting = !isHomeBatting
    val inning        = (outs / 6) + 1

    override def process = {
      case Run => RunEvent
      case Out => OutEvent
    }

    override def apply = {
      case e => if (isHomeBatting) {
        copy(home = home.applyEvent(e))
      } else {
        copy(away = away.applyEvent(e))
      }
    }
  }

  case class TeamAggregate(runs: Int = 0, outs: Int = 0) extends Aggregate[TeamAggregate] {

    val inning = outs / 3

    override def process = {
      case Run => RunEvent
      case Out => OutEvent
    }

    override def apply = {
      case RunEvent => copy(runs = runs + 1)
      case OutEvent => copy(outs = outs + 1)
    }
  }

  def renderBoxScore(game: GameAggregate): Unit = {
    val totalInnings = 9
    (0 until totalInnings).map { inning =>

    }
  }
}
