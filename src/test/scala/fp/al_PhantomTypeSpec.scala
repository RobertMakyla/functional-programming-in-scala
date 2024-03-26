package fp

import fp.al_PhantomType._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class al_PhantomTypeSpec  extends AnyFreeSpec with Matchers{

  "it works" in {
   val door = Door[Closed]()
   door.open.close.open
 }

}
