package fp


import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class aj_TypeClassSpec extends AnyFreeSpec with Matchers{


  "It works for " - {

    "Person" in {
      import fp.aj_TypeClass._
      itShouldBeFalse mustBe false
      itShouldBeTrue mustBe true
    }


  }
}