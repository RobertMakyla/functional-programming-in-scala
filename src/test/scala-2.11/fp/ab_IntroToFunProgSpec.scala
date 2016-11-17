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
      def sizeIsOK(i: Int, s: String): Boolean = s.length == i
      val actual: Int => String => Boolean = curry(sizeIsOK)

      actual(3)("abc") mustBe true
      actual(1)("abc") mustBe false
    }

    "uncurry" in {
      def sizeIsOK(i: Int)(s: String): Boolean = s.length == i
      val actual: (Int, String) => Boolean = uncurry(sizeIsOK)

      actual(3, "abc") mustBe true
      actual(0, "abc") mustBe false
    }

    "compose" in {
      compose(b2c, a2b)("abc") mustBe 3L
    }
    "andThen" in {
      andThen(a2b, b2c)("abc") mustBe 3L
    }

    def a2b(s: String): Int = s.length
    def b2c(i: Int): Long = i.toLong
  }

}
