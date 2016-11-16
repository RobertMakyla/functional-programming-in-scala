package fp

import fp.ab_IntroToFunProg._
import org.scalatest.{FreeSpec, MustMatchers}

class ab_IntroToFunProgSpec extends FreeSpec with MustMatchers {

  "Polymorphic Functions" - {
    import PolymorphicFunctions._

    "isSorted" in {
      def naturally = (a: Int, b: Int) => a < b
      isSorted(Array(), naturally) mustBe true
      isSorted(Array(1), naturally) mustBe true
      isSorted(Array(1, 2, 3), naturally) mustBe true
      isSorted(Array(1, 2, 3, 4), naturally) mustBe true
      isSorted(Array(1, 2, 4, 3), naturally) mustBe false
    }

  }

  "Higher Order Functions" - {
    import HigherOrderFunctions._

    "curry" in {
      def input(i: Int, s: String): Boolean = s.length == i
      val int2String2Boolean: (Int) => (String) => Boolean = curry(input)

      int2String2Boolean(0)("") mustBe true
      int2String2Boolean(1)("a") mustBe true
      int2String2Boolean(2)("ab") mustBe true
      int2String2Boolean(0)("abc") mustBe false

    }
  }

}
