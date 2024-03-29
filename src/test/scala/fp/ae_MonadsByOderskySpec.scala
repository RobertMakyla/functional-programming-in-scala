package fp

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class ae_MonadsByOderskySpec extends AnyFreeSpec with Matchers {

  "Monads by Odersky" - {
    import fp.ae_MonadsByOdersky._

    "Composing kleisli arrows" in {

      val expected = Some(3)
      println("composing Kleisli With For Comprehension")
      composingKleisliWithForComprehension(2) mustBe expected

      println("composing Kleisli By Desugaring For Comprehension")
      composingKleisliByDesugaringForComprehension(2) mustBe expected

      println("composing Kleisli With FlatMaps - completely desugared for-comprehension")
      composingKleisliWithFlatMaps(2) mustBe expected
    }

  }

}