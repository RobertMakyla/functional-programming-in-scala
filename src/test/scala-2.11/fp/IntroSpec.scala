package fp

import org.scalatest.{FreeSpec, MustMatchers}
import fp.Intro._

class IntroSpec extends FreeSpec with MustMatchers{

  "Exercise 2.1 - Fibonacci number" in {
    fib(0) mustBe 0
    fib(1) mustBe 1
    fib(2) mustBe 1
    fib(3) mustBe 2
    fib(4) mustBe 3
    fib(5) mustBe 5
    fib(6) mustBe 8
  }
}
