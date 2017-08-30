package category

import org.scalatest.{MustMatchers, WordSpec}

class CategoryForProgrammersSpec extends WordSpec with MustMatchers {

  "Writer" when {
    "isOdd" should {
      "gives back answer and logs" in {
        val r = CategoryForProgrammers.isOdd(8)
        r mustBe (false, "isEven Not so! ")
      }
    }
    "isOddCompose" should {
      "gives back answer and logs" in {
        val r = CategoryForProgrammers.isOddCompose(8)
        r mustBe (false, "isEven Not so! ")
      }
    }
  }
}
