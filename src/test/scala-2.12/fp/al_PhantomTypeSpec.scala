package fp

import org.scalatest.{FreeSpec, MustMatchers}
import al_PhantomType._
class al_PhantomTypeSpec  extends FreeSpec with MustMatchers{

  "it works" in {
   val door = Door[Closed]()
   door.open.close.open
 }

}
