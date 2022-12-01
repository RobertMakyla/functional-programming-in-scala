package fp

import fp.ad_MonoidsMonads.Monoids.Monoid
import fp.ad_MonoidsMonads._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class ad_MonoidsMonadsSpec extends AnyFreeSpec with Matchers {


  "Monads" - {
    import Monads._

    "implementing Monad's map() using flatMap() and unit()" in {
      map[Int, String](List(1, 2, 3), elem => "" + elem) mustBe
        List("1", "2", "3")
    }

    "testing the simplest monad: Wrapper[A](a: A)" in {
      import fp.ad_MonoidsMonads.Monads.MyWrapper

      for {
        r1 <- MyWrapper("hello")
        r2 <- MyWrapper(r1)
        r3 <- MyWrapper(r2)
      } yield r3 mustBe "hello"

      for {
        r1 <- MyWrapper("hello")
        r2 <- MyWrapper(", world")
        r3 <- MyWrapper(r1 + r2)
      } yield r3 mustBe "hello, world"

    }
    "testing NonEmptyList Monad" in {

      val res = for {
        a <- NonEmptyList(1, 2)
        b <- NonEmptyList(10, 100)
      } yield a * b
      res mustBe NonEmptyList(10, 100, 20, 200)
    }

    "testing MyOption Monad" in {

      val res = for {
        a <- MySome(3)
        b <- MySome(a * 2)
        c <- MySome(b * 5)
      } yield "" + c
      res mustBe MySome("30")

      MySome(3).flatMap(i => MyNone) mustBe MyNone
    }

    "testing MyTry Monad happy path" in {
      val res = for {
        a <- MyTry(3)
        b <- MyTry(a * 2)
        c <- MyTry(b * 5)
      } yield "" + c
      res mustBe MySuccess("30")

    }
    "testing MyTry Monad error catching" in {
      val res = for {
        a <- MyTry(3)
        b <- MyTry.apply[Int]({ throw new Exception("boom")})
        c <- MyTry(b * 5)
      } yield "" + c
      res match {
        case MySuccess(_) => fail("that is unexpected")
        case MyFailure(e) => e.getMessage mustBe "boom"
      }

    }

    "testing Monads associativity" in {

      // for comprehension
      val y: Option[Int] = for {
        a: Int <- Some(10)
        b: Int <- Some(a*2)
        c: Int <- Some(b+3)
      } yield c

      //de-sugared for comprehension
      val y1 =
        Some(10).flatMap(
          a => Some(a * 2).flatMap(
            b => Some(b + 3)
          )
        )

      // flat way
      val y2 = Some(10)
        .flatMap(a => Some(a * 2))
        .flatMap(b => Some(b + 3))

      y mustBe y1
      y1 mustBe y2

    }

  }
}