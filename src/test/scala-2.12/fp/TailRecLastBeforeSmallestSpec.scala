package fp

import TailRecLastBeforeSmallest._
import org.scalatest.{FreeSpec, MustMatchers}

class TailRecLastBeforeSmallestSpec extends FreeSpec with MustMatchers {
  "smallest tail rec" in {
    smallest(List()) mustBe None
    smallest(List(2)) mustBe Some(2)
    smallest(List(1, 2)) mustBe Some(1)
    smallest(List(2, 1)) mustBe Some(1)
    smallest(List(2, 2)) mustBe Some(2)
    smallest(List(1, 2, 3)) mustBe Some(1)
    smallest(List(3, 1, 2)) mustBe Some(1)
    smallest(List(3, 2, 1)) mustBe Some(1)
    smallest(List(1, 2, 2, 3)) mustBe Some(1)
    smallest(List(2, 1, 1, 2, 3)) mustBe Some(1)
    smallest(List(1, 1, 2, 2, 3, 3)) mustBe Some(1)
    smallest(List(1, 2, 3, 4, 5, 6, 7)) mustBe Some(1)
    smallest(List(5, 4, 3, 2, 1)) mustBe Some(1)
  }
  "last before smallest tail rec" in {
    lastBeforeSmallest(List()) mustBe None
    lastBeforeSmallest(List(1)) mustBe None
    lastBeforeSmallest(List(1, 1)) mustBe None
    lastBeforeSmallest(List(1, 2)) mustBe Some(2)
    lastBeforeSmallest(List(2, 1)) mustBe Some(2)
    lastBeforeSmallest(List(1, 2, 3)) mustBe Some(2)
    lastBeforeSmallest(List(1, 3, 2)) mustBe Some(2)
    lastBeforeSmallest(List(3, 2, 1)) mustBe Some(2)
    lastBeforeSmallest(List(1, 2, 2, 3)) mustBe Some(2)
    lastBeforeSmallest(List(1, 1, 2, 3)) mustBe Some(2)
    lastBeforeSmallest(List(1, 1, 2, 2, 3, 3)) mustBe Some(2)
    lastBeforeSmallest(List(1, 2, 3, 4, 5, 6, 7)) mustBe Some(2)
    lastBeforeSmallest(List(1, 2, 3, 4, 5, 6, 7)) mustBe Some(2)
    lastBeforeSmallest(List(5, 4, 3, 2, 1)) mustBe Some(2)
    lastBeforeSmallest(List(5, 4, 3, 2, 1, 2, 1, 5)) mustBe Some(2)
  }
}
