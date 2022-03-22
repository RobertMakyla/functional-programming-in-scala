package fp

import org.scalatest.{FreeSpec, MustMatchers}
import ak_ImplicitEvidence._

class ak_ImplicitEvidenceSpec extends FreeSpec with MustMatchers{

  "it works" in {
    firstAndLast(List(1,2,3)) mustBe (1, 3)
  }

}
