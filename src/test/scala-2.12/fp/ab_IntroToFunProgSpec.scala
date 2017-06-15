package fp

import fp.ab_IntroToFunProg._
import org.scalatest.{FreeSpec, MustMatchers}

import scala.concurrent.Await
import scala.concurrent.duration.FiniteDuration

class ab_IntroToFunProgSpec extends FreeSpec with MustMatchers {

  "Polymorphic Functions" - {
    import PolymorphicFunctions._

    case class TestCase(ls: List[Int], expectedResult: Boolean)
    def naturally = (a: Int, b: Int) => a < b

    "isSorted, isSortedTailRec" in {
      List(
        TestCase(List.empty[Int], true),
        TestCase(List(1), true),
        TestCase(List(1, 2, 3), true),
        TestCase(List(1, 2, 3, 4), true),
        TestCase(List(1, 2, 4, 3), false),
        TestCase(List(1, 2, 4, 4), false),
        TestCase(List(1, 2, 4, 5, 6), true)
      ).foreach { test =>
        isSorted(test.ls, naturally) mustBe test.expectedResult
        isSortedTailRec(test.ls, naturally) mustBe test.expectedResult
      }
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
      foldLeft(List[Int](), 0)(_ + _) mustBe 0
      foldLeft(List(5), 5)(_ + _) mustBe 10
      foldLeft(List(5, 5), 0)(_ + _) mustBe 10
      foldLeft(List(5, 5), 5)(_ + _) mustBe 15
      foldLeft(List(5, 5), 5)(_ * _) mustBe 125
      foldLeft(List("a", "b"), "c")(_ + _) mustBe "cab"
    }
    "reverse() using fold" in {
      reverse(Nil) mustBe Nil
      reverse(List(1)) mustBe List(1)
      reverse(List(1, 2, 3)) mustBe List(3, 2, 1)
    }
    "foldRight() in terms of foldLeft" in {
      foldRight(List("a", "b"), "c")(_ + _) mustBe "abc"
    }

    "append in terms of foldRight" in {
      appendViaFoldRight(Nil, 1) mustBe List(1)
      appendViaFoldRight(List(1, 2, 3), 4) mustBe List(1, 2, 3, 4)
    }

    "append in terms of foldLeft" in {
      appendViaFoldLeft(Nil, 1) mustBe List(1)
      appendViaFoldLeft(List(1, 2, 3), 4) mustBe List(1, 2, 3, 4)
    }

    "flatten" in {
      val input = List(List(1), List(2), List(3, 4), Nil, List(5))
      val output = List(1, 2, 3, 4, 5)

      flatten1(input) mustBe output
      flatten2(input) mustBe output
      flatten3(input) mustBe output
      flatten4(input) mustBe output
    }
    "map via fold" in {
      map(List(1, 2, 3))("" + _) mustBe List("1", "2", "3")
    }
    "filter" in {
      filter(List(1, 2, 3, 4, 5, 6))(_ % 2 == 0) mustBe List(2, 4, 6)
    }
    "flatMap" in {
      flatMap(List(1, 2, 3))(i => List(i, i)) mustBe List(1, 1, 2, 2, 3, 3)
    }
    "filter via fatMap" in {
      filterViaFlatMap(List(1, 2, 3, 4, 5, 6))(_ % 2 == 0) mustBe List(2, 4, 6)
    }
    "zip for two lists of Ints" in {
      zip(List(1, 2, 3), List(4, 5, 6)) mustBe List(5, 7, 9)
      zip(List(1, 2, 3), List(100)) mustBe List(101, 2, 3)
      zip(List(100), List(1, 2, 3)) mustBe List(101, 2, 3)
    }
    "zipWith" in {
      zipWith(List(1, 2, 3), List(4, 5, 6))(_ + _) mustBe List(5, 7, 9)
      zipWith(List(1, 2, 3), List(100))(_ + _) mustBe List(101, 2, 3)
      zipWith(List(100), List(1, 2, 3))(_ + _) mustBe List(101, 2, 3)
    }
    "hasSubsequence" in {
      //impossible due to size difference
      hasSubsequence(Nil, List(1)) mustBe false
      hasSubsequence(List(1), List(1, 2)) mustBe false
      //possible due to size difference
      hasSubsequence(Nil, Nil) mustBe true
      hasSubsequence(List(1, 2, 3, 4), Nil) mustBe true
      hasSubsequence(List(1, 2, 3, 4), List(2)) mustBe true
      hasSubsequence(List(1, 2, 3, 4), List(2, 4)) mustBe false
      hasSubsequence(List(1, 2, 3, 4), List(3, 4)) mustBe true
      hasSubsequence(List(1, 2, 3, 4), List(1, 2, 3, 4)) mustBe true
    }

    val tree = Branch(Leaf(1), Branch(Branch(Leaf(2), Leaf(3)), Branch(Leaf(9), Branch(Leaf(2), Leaf(3)))))

    "tree size" in {
      FunctionalDataStructures.size(tree) mustBe 11
      FunctionalDataStructures.sizeViaFold(tree) mustBe 11
    }
    "tree max" in {
      FunctionalDataStructures.max(tree) mustBe 9
      FunctionalDataStructures.maxViaFold(tree) mustBe 9
    }
    "tree depth" in {
      FunctionalDataStructures.depth(tree) mustBe 5
      FunctionalDataStructures.depthViaFold(tree) mustBe 5
    }
    "tree map" in {
      FunctionalDataStructures.map(tree)("" + _) mustBe
        Branch(Leaf("1"), Branch(Branch(Leaf("2"), Leaf("3")), Branch(Leaf("9"), Branch(Leaf("2"), Leaf("3")))))

      FunctionalDataStructures.mapViaFold(tree)("" + _) mustBe
        Branch(Leaf("1"), Branch(Branch(Leaf("2"), Leaf("3")), Branch(Leaf("9"), Branch(Leaf("2"), Leaf("3")))))
    }

  }

  "Handling Errors Without Exceptions" - {
    import HandlingErrorsWithoutExceptions._

    "Option" - {
      "map(), flatMap(), filter" in {
        MySome(123).map(_ * 100) mustBe MySome(12300)
        MySome(123).flatMap(i => MySome(i * 100)) mustBe MySome(12300)
        MySome(123).filter(_ > 100) mustBe MySome(123)
        MySome(123).filter(_ < 100) mustBe MyNone

        MySome(123).getOrElse(0) mustBe 123
        (MyNone: MyOption[Int]).getOrElse(0) mustBe 0

        MySome(123).orElse(MySome(4)) mustBe MySome(123)
        (MyNone: MyOption[Int]).orElse(MySome(4)) mustBe MySome(4)
      }

      "map2() combining 2 monads into one" in {
        map2(Some("abc"), Some(2))((a: String, b: Int) => a(b)) mustBe Some('c')
        map2(Some("abc"), None)((a: String, b: Int) => a(b)) mustBe None

        map2ViaForComprehension(Some("abc"), Some(2))((a: String, b: Int) => a(b)) mustBe Some('c')
        map2ViaForComprehension(Some("abc"), None)((a: String, b: Int) => a(b)) mustBe None
      }

      "sequence() transforming List[Option[A]] to Option[List[A]]" in {
        sequence(List(Some(1), Some(2), Some(3))) mustBe Some(List(1, 2, 3))
        sequence(List(Some(1), Some(2), None)) mustBe None
      }

      "sequence() with for-comprehension transforming List[Option[A]] to Option[List[A]]" in {
        sequenceViaForComprehension(List(Some(1), Some(2), Some(3))) mustBe Some(List(1, 2, 3))
        sequenceViaForComprehension(List(Some(1), Some(2), None)) mustBe None
      }

    }
    "Either" - {
      "map(), flatMap(), map2() combining 2 monads into one" in {
        val r: MyEither[String, Int] = MyRight(123)
        val rr: MyEither[String, Int] = MyRight(10)
        val l: MyEither[String, Int] = MyLeft("error")

        r.map(_ * 100) mustBe MyRight(12300)
        l.map(_ * 100) mustBe MyLeft("error")

        r.flatMap(i => MyRight(i * 100)) mustBe MyRight(12300)
        r.flatMap(i => MyLeft("" + i)) mustBe MyLeft("123")

        l.flatMap(i => MyRight(i * 100)) mustBe MyLeft("error")
        l.flatMap(i => MyLeft("" + i)) mustBe MyLeft("error")

        r.map2(l)(_ * _) mustBe MyLeft("error")
        r.map2(rr)(_ * _) mustBe MyRight(1230)
      }

      "sequence() transforming List[Either[ERR,STH]] to Either[ERR, List[STH]]" in {
        sequence(List(MyRight(1), MyRight(2), MyRight(3))) mustBe MyRight(List(1, 2, 3))
        sequence(List(MyRight(1), MyRight(2), MyLeft("err"))) mustBe MyLeft("err")
      }

      "sequence() with for-comprehension transforming List[Either[ERR,STH]] to Either[ERR, List[STH]]" in {
        sequenceViaForComprehension(List(MyRight(1), MyRight(2), MyRight(3))) mustBe MyRight(List(1, 2, 3))
        sequenceViaForComprehension(List(MyRight(1), MyRight(2), MyLeft("err"))) mustBe MyLeft("err")
      }

    }
  }

  "For Comprehension (under the hood)" - {
    import forComprehensionUnderTheHood._

    "with Option" in {
      isForComprehensionOptionWorking mustBe true
    }

    "with Either (since scala 2.12 Either has right-biased flatMap)" in {
      isForComprehensionEitherWorking mustBe true
    }

  }

  "Streams" - {
    import Streams._
    "converting a Stream to a List" in {
      toList(MyStream[Int](1, 2, 3)) mustBe List(1, 2, 3)
      toListTailRec(MyStream[Int](1, 2, 3)) mustBe List(1, 2, 3)
    }
    "take(n) from Stream" in {
      take(MyStream[Int](1, 2, 3), 0) mustBe List()
      take(MyStream[Int](1, 2, 3), 1) mustBe List(1)
      take(MyStream[Int](1, 2, 3), 2) mustBe List(1, 2)
      take(MyStream[Int](1, 2, 3), 3) mustBe List(1, 2, 3)
    }
    "takeWhile(p: A => Boolean) from Stream" in {
      toList(takeWhile[Int](MyStream[Int](1, 2, 3, 4), _ <= 3)) mustBe List(1, 2, 3)
    }

    "exists(p: A => Boolean) from Stream" in {
      exists[Int](MyStream[Int](0, 2, 4, 6), _ % 2 != 0) mustBe false
    }
    "forAll(p: A => Boolean) from Stream" in {
      forAll[Int](MyStream[Int](), _ > 10) mustBe true
      forAll[Int](MyStream[Int](11, 12), _ > 10) mustBe true
      forAll[Int](MyStream[Int](11, 0, 12), _ > 10) mustBe false
    }
    "Infinite Stream of ones (1, 1, 1, 1 ... )" in {
      take(ones, 5) mustBe List(1, 1, 1, 1, 1)
      exists[Int](ones, _ == 1) mustBe true

      take(infiniteStreamOf[Boolean](true), 5) mustBe List(true, true, true, true, true)
      take(from(0), 5) mustBe List(0, 1, 2, 3, 4)
    }
    "Infinitif Fibonacci numbers" in {
      take(fibs(), 7) mustBe List(0, 1, 1, 2, 3, 5, 8)
    }
  }

  "Purely Functional State" - {
    import PurelyFunctionalState._

    "generate randonly list of integers" in {
      val rng = SimpleRNG(123)

      ints(10)(rng) mustBe ints(10)(rng)
    }
  }

  "Futures" - {
    import scala.concurrent.duration._
    import Futures._

    val oneSec = 1.seconds

    "global ExecutionContext - THREAD POOL of 4 threads" in {
      val numberOfProcessors = Runtime.getRuntime().availableProcessors()
      numberOfProcessors mustBe 4
    }

    "asynchronous future" in {
      Await.result(asyncFuture, oneSec) mustBe UpperCaseName("ASYNC_FUTURE")
    }

    "synchronous future - one after another" in {
      Await.result(syncFuture, oneSec) mustBe UpperCaseNameWithSpaces("S Y N C _ F U T U R E")
    }

  }

}