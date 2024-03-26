package fp

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import fp.aa_TailRec._

class aa_TailRecSpec extends AnyFreeSpec with Matchers{

  "Factorial" - {
    "direct way" in {
      factorial(1) mustBe 1
      factorial(2) mustBe 2
      factorial(3) mustBe 6
      factorial(4) mustBe 24
    }

//    "continuation passing style" in {
//      factorialCPS(1) mustBe 1
//      factorialCPS(2) mustBe 2
//      factorialCPS(3) mustBe 6
//      factorialCPS(4) mustBe 24
//    }
  }

  "Sum" - {
    "direct way" in {
      sum(0) mustBe 0
      sum(1) mustBe 1
      sum(2) mustBe 3
      sum(3) mustBe 6
      sum(4) mustBe 10
    }
//    "continuation passing style" in {
//      sumCPS(0) mustBe 0
//      sumCPS(1) mustBe 1
//      sumCPS(2) mustBe 3
//      sumCPS(3) mustBe 6
//      sumCPS(4) mustBe 10
//    }
  }

  "Fibonacci number" in {
    fib(1) mustBe 0
    fib(2) mustBe 1
    fib(3) mustBe 1
    fib(4) mustBe 2
    fib(5) mustBe 3
    fib(6) mustBe 5
    fib(7) mustBe 8
    fib(8) mustBe 13
  }

//  "Mouse - continuation passing style" in {
//    mouseCPS(0) mustBe 1
//    mouseCPS(1) mustBe 1
//    mouseCPS(2) mustBe 1
//    mouseCPS(3) mustBe 2
//    mouseCPS(4) mustBe 3
//    mouseCPS(5) mustBe 4
//    mouseCPS(6) mustBe 9
//    mouseCPS(7) mustBe 28
//  }

}