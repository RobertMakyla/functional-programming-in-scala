package fp

import org.scalatest.{FreeSpec, MustMatchers}

class ah_IOMonadSpec extends FreeSpec with MustMatchers {

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