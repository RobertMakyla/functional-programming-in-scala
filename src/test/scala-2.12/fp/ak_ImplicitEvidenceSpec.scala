package fp

import fp.ak_ImplicitEvidence._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class ak_ImplicitEvidenceSpec extends AnyFreeSpec with Matchers{

  "it works" in {
    firstAndLast(List(1,2,3)) mustBe (1, 3)
  }

}
