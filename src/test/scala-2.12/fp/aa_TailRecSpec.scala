package fp

import org.scalatest.{FreeSpec, MustMatchers}
import fp.aa_TailRec._

class aa_TailRecSpec extends FreeSpec with MustMatchers{

  "Factorial" - {
    "direct way" in {
      factorialDirectWay(1) mustBe 1
      factorialDirectWay(2) mustBe 2
      factorialDirectWay(3) mustBe 6
      factorialDirectWay(4) mustBe 24
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
      sumDirectWay(0) mustBe 0
      sumDirectWay(1) mustBe 1
      sumDirectWay(2) mustBe 3
      sumDirectWay(3) mustBe 6
      sumDirectWay(4) mustBe 10
    }
//    "continuation passing style" in {
//      sumCPS(0) mustBe 0
//      sumCPS(1) mustBe 1
//      sumCPS(2) mustBe 3
//      sumCPS(3) mustBe 6
//      sumCPS(4) mustBe 10
//    }
  }

  "Fibonacci number - continuation passing style" in {
    fibCPS(0) mustBe 1
    fibCPS(1) mustBe 1
    fibCPS(2) mustBe 2
    fibCPS(3) mustBe 3
    fibCPS(4) mustBe 5
    fibCPS(5) mustBe 8
    fibCPS(6) mustBe 13
  }

  "Mouse - continuation passing style" in {
    mouseCPS(0) mustBe 1
    mouseCPS(1) mustBe 1
    mouseCPS(2) mustBe 1
    mouseCPS(3) mustBe 2
    mouseCPS(4) mustBe 3
    mouseCPS(5) mustBe 4
    mouseCPS(6) mustBe 9
    mouseCPS(7) mustBe 28
  }

}