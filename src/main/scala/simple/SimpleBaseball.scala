package simple

import simple.SimpleBaseball.Box.{BoxItem, BoxScore}

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

  object Box {

    case class BoxColumn(header: String, away: String, home: String, separator: String = "-") {
      lazy val padd = Seq(header,away,home,separator).map(_.length).max
      def padded(f: BoxColumn => String): String = paddRight(f(this),padd)
    }
    case class BoxItem(away: Int, home: Int)

    case class BoxScore(box: List[BoxItem], runs: BoxItem)

    def digits(n: Int) = if (n == 0) 1 else math.log10(math.abs(n)).toInt + 1

    def boxAsString(bs: BoxScore): String = {
      val names = BoxColumn("","Away","Home")
      val innings = bs.box.zipWithIndex.map{case (boxItem, index) =>
        BoxColumn((index + 1).toString,boxItem.away.toString,boxItem.home.toString)
      }
      val agg = Seq(BoxColumn("R",bs.runs.away.toString,bs.runs.home.toString))
      val columnSep = BoxColumn("|","|","|","-")
      val all = Seq(names) ++ innings ++ agg

      Seq(
        all.map(_.padded(_.header)).mkString(" "),
        all.map(_.padded(_.separator)).mkString("-"),
        all.map(_.padded(_.away)).mkString(" "),
        all.map(_.padded(_.home)).mkString(" "),
        all.map(_.padded(_.separator)).mkString("-")
      ).mkString("\n")
    }

    def boxAsStrin2g(bs: BoxScore): String = {
      val innings = if (bs.box.length < 9) 9 else bs.box.length
      val header = (1 until innings + 1).map(i => i -> digits(i))
      val away = bs.box.map(bi => Some(bi.away))
      val home = bs.box.map(bi => Some(bi.home))

      val h = header.map(_._1).mkString(" ") + " "
      val diff = 9 - away.length
      val fillLine = fillArr(diff,None)

      val awayRow = (away ++ fillLine).map(_.getOrElse(" ")).mkString(" ") + " "
      val homeRow = (home ++ fillLine).map(_.getOrElse(" ")).mkString(" ") + " "
      val separator = fill(awayRow.length , "-")
      val boxSep = "|"
      val names = buildNames(bs)
      val box = buildBox(bs)
      val agg = buildAgg(bs)
      val zipped = names.zip(box).zip(agg)
      zipped.map {
        case ((names,box),agg) => s"$names $box $agg"
      }.mkString("\n")
//      zipped.map
//      Seq(
//        boxSep + " " + h + boxSep + " R |",
//        "--" + separator + "-----",
//        boxSep + " " + awayRow + boxSep + " " + bs.runs.away + " |",
//        boxSep + " " + homeRow + boxSep + " " + bs.runs.home + " |",
//        "--" + separator + "-----"
//      ).mkString("\n")
    }

    def buildNames(bs: BoxScore): Seq[String] = {
      val names = Seq("Away","Home")
      val largest = names.map(_.length).max
      Seq(
        fill(largest," "),
        fill(largest,"-")
      ) ++ names ++ Seq(
        fill(largest,"-")
      )
    }

    def buildBox(bs: BoxScore): Seq[String] = {
      val innings = if (bs.box.length < 9) 9 else bs.box.length
      val header = (1 until innings + 1).map(i => i -> digits(i))
      val away = bs.box.map(bi => Some(bi.away))
      val home = bs.box.map(bi => Some(bi.home))

      val h = header.map(_._1).mkString(" ")
      val diff = 9 - away.length
      val fillLine = fillArr(diff,None)

      val awayRow = (away ++ fillLine).map(_.getOrElse(" ")).mkString(" ")
      val homeRow = (home ++ fillLine).map(_.getOrElse(" ")).mkString(" ")
      val separator = fill(awayRow.length , "-")
      val largest = Seq(h,awayRow,homeRow).map(_.length).max
      Seq(
        h,
        fill(largest,"-"),
        awayRow,
        homeRow,
        fill(largest,"-")
      )
    }

    def buildAgg(bs: BoxScore): Seq[String] = {
      val h = "R"
      val aggs = Seq(bs.runs.away,bs.runs.home).map(_.toString)
      val largest = aggs.map(_.length).max
      Seq(
        h,
        fill(largest,"-")
      ) ++ aggs ++ Seq(
        fill(largest,"-")
      )
    }

    def fillArr[A](n: Int, a: A): Seq[A] = (0 until n).map(_ => a)

    def fill(n: Int, s: String): String =
      fillArr(n,s).mkString("")

    def padd(s: String, p: String): String = s"$p$s$p"
    def paddRight(s: String,n: Int): String = {
      val diff = n - s.length
      s + fill(diff," ")
    }

    val bs =
      """
        |      | 1 2 3 4 5 6 7 8 9 | R H E |
        |-----------------------------------
        | Away | 0 0 0 1 0 2 0 0 1 | 4 8 0 |
        | Home | 1 1 1 3 0 0 0 0 0 | 6 9 0 |
        |-----------------------------------
      """.stripMargin
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
