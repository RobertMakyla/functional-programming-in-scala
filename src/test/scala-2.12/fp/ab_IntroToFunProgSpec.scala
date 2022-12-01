package fp

import fp.ab_IntroToFunProg._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success}

class ab_IntroToFunProgSpec extends AnyFreeSpec with Matchers {

  "Polymorphic Functions" - {
    import PolymorphicFunctions._

    case class TestCase(ls: List[Int], expectedResult: Boolean)

    def naturally = (a: Int, b: Int) => a < b

    "isSorted" in {
      List(
        TestCase(List.empty[Int], true),
        TestCase(List(1), true),
        TestCase(List(1, 2, 3), true),
        TestCase(List(1, 2, 3, 4), true),
        TestCase(List(1, 2, 4, 3), false),
        TestCase(List(1, 2, 4, 4), false),
        TestCase(List(1, 2, 4, 4, 5), false),
        TestCase(List(1, 2, 4, 5, 6), true),
        TestCase(List(1, 2, 4, 5, 7), true),
        TestCase(List(1, 2, 4, 5, 8), true)
      ).foreach { test =>
        isSorted(test.ls, naturally) mustBe test.expectedResult
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

    //    "compose" in {
    //      compose(b2c, a2b)("abc") mustBe 3L
    //    }
    //    "andThen" in {
    //      andThen(a2b, b2c)("abc") mustBe 3L
    //    }

    def a2b(s: String): Int = s.length

    def b2c(i: Int): Long = i.toLong
  }

  "Functional Data Structures" - {
    import FunctionalDataStructures._
    //    "tail" in {
    //      tail(List(1, 2, 3)) mustBe List(2, 3)
    //      intercept[IllegalArgumentException](tail(Nil)).getMessage mustBe "list should not be empty"
    //    }
    //    "head" in {
    //      setHead(1, List(2, 3)) mustBe List(1, 2, 3)
    //      setHead(1, Nil) mustBe List(1)
    //    }
    //    "drop" in {
    //      drop(List(1, 2, 3), 1) mustBe List(2, 3)
    //      drop(List(1, 2, 3), 3) mustBe Nil
    //      intercept[IllegalArgumentException](drop(List(1, 2), 3)).getMessage mustBe "list is too small"
    //    }
    //    "dropWhile" in {
    //      dropWhile[Int](List(1, 2, 3, 4, 5, 6, 7), _ % 2 == 0) mustBe List(1, 3, 5, 7)
    //      dropWhile[String](List("a", "bb", "ccc", "dd"), _.length >= 2) mustBe List("a")
    //    }
    //    "init" in {
    //      init(List(1, 2, 3, 4, 5)) mustBe List(1, 2, 3, 4)
    //      init(List(1, 2)) mustBe List(1)
    //      init(List(1)) mustBe Nil
    //      intercept[IllegalArgumentException](init(Nil)).getMessage mustBe "list should not be empty"
    //    }

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

    //    "append in terms of foldRight" in {
    //      appendViaFoldRight(Nil, 1) mustBe List(1)
    //      appendViaFoldRight(List(1, 2, 3), 4) mustBe List(1, 2, 3, 4)
    //    }
    //
    //    "append in terms of foldLeft" in {
    //      appendViaFoldLeft(Nil, 1) mustBe List(1)
    //      appendViaFoldLeft(List(1, 2, 3), 4) mustBe List(1, 2, 3, 4)
    //    }

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
    "append " in {
      appendWithFoldLeft(Nil, 2) mustBe List(2)
      appendWithFoldLeft(List(1), 2) mustBe List(1,2)
      appendWithFoldLeft(List(1,3,4), 2) mustBe List(1,3,4,2)
      appendWithFoldRight(Nil, 2) mustBe List(2)
      appendWithFoldRight(List(1), 2) mustBe List(1, 2)
      appendWithFoldRight(List(1, 3, 4), 2) mustBe List(1, 3, 4, 2)
    }
    "prenend " in {
      prependWithFoldLeft(Nil, 2) mustBe List(2)
      prependWithFoldLeft(List(1), 2) mustBe List(2, 1)
      prependWithFoldLeft(List(1, 3, 4), 2) mustBe List(2, 1, 3, 4)
      prependWithFoldRight(Nil, 2) mustBe List(2)
      prependWithFoldRight(List(1), 2) mustBe List(2, 1)
      prependWithFoldRight(List(1, 3, 4), 2) mustBe List(2, 1, 3, 4)
    }

    "collect " in {
      val f: PartialFunction[Int, String] = {
        case x if (x % 2) == 0 => (x * 100).toString
      }
      collect(List(1,2,3,4), f) mustBe List("200", "400")
    }

  }

  "Tree" in {
    import MyTree._
    val t =
      Branch(Leaf(1),
        Branch(Leaf(1),
          Branch(Branch(Leaf(1), Branch(Leaf(1), Leaf(1))),
            Branch(Leaf(1),
              Branch(Leaf(1),
                Branch(Leaf(1),
                  Branch(Leaf(1),
                    Leaf(1))))))))
    height(t) mustBe 8
  }
  "For Comprehension (under the hood)" in {
    import forComprehensionUnderTheHood._

    resultForComrehension mustBe resultDesugared
    resultForComrehension mustBe Some(12300)
  }

  "Futures" - {
    import scala.concurrent.duration._
    import Futures._

    val oneSec = 1.seconds

    val ec = scala.concurrent.ExecutionContext.global

    "global ExecutionContext - THREAD POOL of 4 threads" in {
      val numberOfProcessors = Runtime.getRuntime().availableProcessors()
      numberOfProcessors mustBe 4
    }

    "asynchronous future" in {
      Await.result(newAsyncFuture(ec), oneSec) mustBe UpperCaseName("ASYNC_FUTURE")
    }

    "failing future encapsulates errors/exceptions in separate thread, so no exception here " in {
      val ec = scala.concurrent.ExecutionContext.global

      val aFailingFuture: Future[String] = newFailingFuture(ec)
      aFailingFuture.onComplete { case Failure(_) => throw new RuntimeException("some handling failure crashed ") }(ec)
      Await.ready(aFailingFuture, oneSec)
    }

//    "callbacks - eventually all are executed, order not define, regardless errors in other callbacks" in {
//      import scala.concurrent.ExecutionContext.Implicits.global
//
//      val aFuture: Future[String] = Future("a" * 10)
//      aFuture onSuccess { case _ => throw new RuntimeException("some handling success crashed") }
//      aFuture onFailure { case _ => throw new RuntimeException("some handling failure crashed") }
//      aFuture onSuccess { case _ => print(" I can register many callbacks on 1 future") }
//      aFuture onSuccess { case _ => print(" callbacks are executed regardless errors in other callbacks") }
//      aFuture onSuccess { case _ => print(" as long as Future completes as Success/Failure and matches type in PartialFunction, eg Exception type in onFailure()") }
//      aFuture onSuccess { case _ => print(" and as long as callback gets Thread from pool to be executed") }
//    }

    "synchronous future - one after another" in {
      Await.result(newSyncFuture(ec), oneSec) mustBe UpperCaseNameWithSpaces("S Y N C _ F U T U R E")
    }

    "future projection" in {
      import scala.concurrent.ExecutionContext.Implicits.global
      val ec = scala.concurrent.ExecutionContext.global

      newFailingFuture(ec).failed //it's OK that it's newFailingFuture, cause we expect it to fail
        .onComplete {
          case Success(t: Throwable) => // ok
          case _ => fail()
        }

      newAsyncFuture(ec).failed // it's successful future, but we expect it to fail..
        .onComplete {
          case Failure(t: NoSuchElementException) => //ok
          case _ => fail()
        }

    }

    "Promise - container for completed futures with possibility to change result" in {
      import scala.concurrent.ExecutionContext.Implicits.global

      newFutureFromPromise("Robert")(shouldPass = true).onComplete {
        case Success(res: String) => // ok
        case Failure(t) => fail()
      }

      newFutureFromPromise("Robert")(shouldPass = false).onComplete {
        case Success(res: String) => fail()
        case Failure(t) => // ok
      }

    }

  }

}