package fp

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class ah_IOMonadSpec extends AnyFreeSpec with Matchers {

  import ah_IOMonad._

  "IO Monad" - {
    "creating IO Monad - not running yet" in {
      forComprehensionIO
    }

    "creating and running IO Monad" in {
      forComprehensionIO.run

      twoHundred.run mustBe 200
    }

  }

}