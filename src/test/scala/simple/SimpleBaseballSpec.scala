package simple

import org.scalatest.{MustMatchers, WordSpec}
import SimpleBaseball.Box
import SimpleBaseball.Box.{BoxItem, BoxScore}

class SimpleBaseballSpec extends WordSpec with MustMatchers {

  "Box" should {
    "generate simple box score for complete game" in {
      val boxScore = BoxScore(List(
        BoxItem(0,0),
        BoxItem(1,0),
        BoxItem(1,1),
        BoxItem(2,0),
        BoxItem(0,3),
        BoxItem(0,4),
        BoxItem(2,0),
        BoxItem(0,3),
        BoxItem(0,0)
      ),BoxItem(1,2))
//      println(Box.boxAsString(boxScore))
    }

    "generate simple box score for partial game" in {
      val boxScore = BoxScore(List(
        BoxItem(0,0),
        BoxItem(1,0),
        BoxItem(1,1),
        BoxItem(2,0),
        BoxItem(0,3)
      ),BoxItem(4,4))
      println(Box.boxAsString(boxScore))
    }
  }

  "Process Commands" should {
    "blah" in {
//      SimpleBaseball.process
    }
  }
}
