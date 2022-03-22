package fp


import org.scalatest.{FreeSpec, MustMatchers}

class aj_TypeClassSpec extends FreeSpec with MustMatchers{
  import fp.aj_TypeClass._

  "It works for " - {

    "Ints" in {
      (1 ajoute 2) mustBe 3
    }

    "String" in {
      "a" ajoute "b" mustBe "ab"
    }

  }
}