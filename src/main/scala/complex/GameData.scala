package complex

import complex.Things._
import complex.Baseball._

object GameData {

  object Giants {

    object Players {
      //Starting lineup
      //Batters
      val GregorBlanco     = Player("Gregor Blanco", 7, AwayTeam)
      val JoePanik         = Player("Joe Panik", 12, AwayTeam)
      val BusterPosey      = Player("Buster Posey", 28, AwayTeam)
      val PabloSandavol    = Player("Pablo Sandavol", 48, AwayTeam)
      val HunterPence      = Player("Hunter Pence", 8, AwayTeam)
      val BrandonBelt      = Player("Brandon Belt", 9, AwayTeam)
      val MichaelMorse     = Player("Michael Morse", 38, AwayTeam)
      val BrandonCrawford  = Player("Brandon Crawford", 35, AwayTeam)
      val JuanPerez        = Player("Juan Perez", 2, AwayTeam)
      //Pitchers
      val TimHudson        = Player("Tim Hudson", 3, AwayTeam)
      val JeremyAfeldt     = Player("Jeremy Afeldt", 41, AwayTeam)
      val MadisonBumgarner = Player("Madison Bumgarner", 40, AwayTeam)
    }

  }

  object Royals {

    object Players {
      //Batters
      val Escobar   = Player("Escobar", 1, HomeTeam)
      val Aoki      = Player("Aoki", 1, HomeTeam)
      val Cain      = Player("Cain", 1, HomeTeam)
      val Hosmer    = Player("Hosmer", 1, HomeTeam)
      val Butler    = Player("Butler", 1, HomeTeam)
      val Gordon    = Player("Gordon", 1, HomeTeam)
      val Perez     = Player("Perez", 1, HomeTeam)
      val Moustakas = Player("Moustakas", 1, HomeTeam)
      val Infante   = Player("Infante", 1, HomeTeam)
      //Pitchers
      val Guthrie   = Player("Guthrie", 1, HomeTeam)
    }

  }

  val gameLog = List(
    //Away team
    PlayerActive(Giants.Players.GregorBlanco, CenterField),
    PlayerActive(Giants.Players.JoePanik, SecondBase),
    PlayerActive(Giants.Players.BusterPosey, Catcher),
    PlayerActive(Giants.Players.PabloSandavol, ThirdBase),
    PlayerActive(Giants.Players.HunterPence, RightField),
    PlayerActive(Giants.Players.BrandonBelt, FirstBase),
    PlayerActive(Giants.Players.MichaelMorse, DesignatedHitter),
    PlayerActive(Giants.Players.BrandonCrawford, ShortStop),
    PlayerActive(Giants.Players.JuanPerez, LeftField),
    PlayerActive(Giants.Players.TimHudson, Pitcher),
    //Home team
    PlayerActive(Royals.Players.Escobar, ShortStop),
    PlayerActive(Royals.Players.Aoki, RightField),
    PlayerActive(Royals.Players.Cain, CenterField),
    PlayerActive(Royals.Players.Hosmer, FirstBase),
    PlayerActive(Royals.Players.Butler, DesignatedHitter),
    PlayerActive(Royals.Players.Gordon, LeftField),
    PlayerActive(Royals.Players.Perez, Catcher),
    PlayerActive(Royals.Players.Moustakas, ThirdBase),
    PlayerActive(Royals.Players.Infante, SecondBase),
    PlayerActive(Royals.Players.Guthrie, Pitcher),
    //Top 1st
    PitchToPlayer(ChangeUp, 89),
    PitchOutcome(CalledStrike),
    PitchToPlayer(ChangeUp, 83),
    PitchOutcome(SwingingStrike),
    PitchToPlayer(ChangeUp, 84),
    PitchOutcome(Ball),
    PitchToPlayer(Sinker, 93),
    PitchOutcome(CalledStrike),
    PitchToPlayer(Cutter, 91),
    PitchOutcome(Struck),
    PlayerOut(List(CenterField), FlyOut),
    PitchToPlayer(ChangeUp, 90),
    PitchOutcome(Ball),
    PitchToPlayer(ChangeUp, 84),
    PitchOutcome(CalledStrike),
    PitchToPlayer(ChangeUp, 85),
    PitchOutcome(Struck),
    PlayerOut(List(FirstBase, Pitcher), GroundOut),
    PitchToPlayer(Cutter, 89),
    PitchOutcome(Foul),
    PitchToPlayer(Sinker, 92),
    PitchOutcome(Struck),
    PlayerOut(List(ThirdBase, FirstBase), GroundOut)
    //End bottom 1
  )

}
