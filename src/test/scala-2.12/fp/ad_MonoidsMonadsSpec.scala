package fp

import fp.ad_MonoidsMonads.Monoids.Monoid
import fp.ad_MonoidsMonads._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck._
import org.scalatest.{FreeSpec, MustMatchers}

class ad_MonoidsMonadsSpec extends FreeSpec with MustMatchers {

  "Monoids" - {
    import Monoids.stringConcat._

    "String, +, \"\" - operation must be associative" in {
      op(op("a", "b"), "c") mustBe op("a", op("b", "c"))
    }
    "String, +, \"\" - zero element must be applicable anywhere, not changing result" in {
      op("a", zero) mustBe "a"
      op(zero, "a") mustBe "a"
    }

    "composing Monoids into products" in {
      import Monoids.productMonoid
      import Monoids.intMultiplication
      import Monoids.stringConcat

      val x = (100, "hello")
      val y = (200, "world")

      val productM: Monoid[(Int, String)] = productMonoid(intMultiplication, stringConcat)

      productM.op(x, y) mustBe (20000, "helloworld")
      productM.zero mustBe (1, "")
    }
  }

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
  }
}

object MonoidLaws extends Properties("monoid String, +, \"\" ") {

  import Monoids.stringConcat._

  val genString: Gen[String] = arbitrary[Char].map(_.toString)

  property("associativity") =
    forAll(genString, genString, genString) {
      (s1: String, s2: String, s3: String) => op(op(s1, s2), s3) == op(s1, op(s2, s3))
    }
  property("identity element") =
    forAll(genString) {
      (s: String) => op(s, zero) == s && op(zero, s) == s
    }
}