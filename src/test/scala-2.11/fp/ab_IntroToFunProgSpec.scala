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

  "Functional Data Structures" - {
    import FunctionalDataStructures._
    "tail" in {
      tail(List(1, 2, 3)) mustBe List(2, 3)
      intercept[IllegalArgumentException](tail(Nil)).getMessage mustBe "list should not be empty"
    }
    "head" in {
      setHead(1, List(2, 3)) mustBe List(1, 2, 3)
      setHead(1, Nil) mustBe List(1)
    }
    "drop" in {
      drop(List(1, 2, 3), 1) mustBe List(2, 3)
      drop(List(1, 2, 3), 3) mustBe Nil
      intercept[IllegalArgumentException](drop(List(1, 2), 3)).getMessage mustBe "list is too small"
    }
    "dropWhile" in {
      dropWhile[Int](List(1, 2, 3, 4, 5, 6, 7), _ % 2 == 0) mustBe List(1, 3, 5, 7)
      dropWhile[String](List("a", "bb", "ccc", "dd"), _.length >= 2) mustBe List("a")
    }
    "init" in {
      init(List(1, 2, 3, 4, 5)) mustBe List(1, 2, 3, 4)
      init(List(1, 2)) mustBe List(1)
      init(List(1)) mustBe Nil
      intercept[IllegalArgumentException](init(Nil)).getMessage mustBe "list should not be empty"
    }
    "length() using foldRight" in {
      FunctionalDataStructures.length(Nil) mustBe 0
      FunctionalDataStructures.length(List(1)) mustBe 1
      FunctionalDataStructures.length(List(1, 2, 3)) mustBe 3
    }
    "foldLeft() tail recursive" in {
      FunctionalDataStructures.foldLeft(List[Int](), 0)(_ + _) mustBe 0
      FunctionalDataStructures.foldLeft(List(5), 5)(_ + _) mustBe 10
      FunctionalDataStructures.foldLeft(List(5, 5), 0)(_ + _) mustBe 10
      FunctionalDataStructures.foldLeft(List(5, 5), 5)(_ + _) mustBe 15
      FunctionalDataStructures.foldLeft(List(5, 5), 5)(_ * _) mustBe 125
      FunctionalDataStructures.foldLeft(List("a", "b"), "c")(_ + _) mustBe "cab"
    }
    "reverse() using fold" in {
      FunctionalDataStructures.reverse(Nil) mustBe Nil
      FunctionalDataStructures.reverse(List(1)) mustBe List(1)
      FunctionalDataStructures.reverse(List(1, 2, 3)) mustBe List(3, 2, 1)
    }
    "foldRight() in terms of foldLeft" in {
      foldRightViaFoldLeft(List("a", "b"), "c")(_ + _) mustBe "abc"
    }

    "append in terms of foldRight" in {
      appendViaFoldRight(Nil, 1) mustBe List(1)
      appendViaFoldRight(List(1, 2, 3), 4) mustBe List(1, 2, 3, 4)
    }

    "append in terms of foldLeft" in {
      appendViaFoldLeft(Nil, 1) mustBe List(1)
      appendViaFoldLeft(List(1, 2, 3), 4) mustBe List(1, 2, 3, 4)
    }

  }

}
