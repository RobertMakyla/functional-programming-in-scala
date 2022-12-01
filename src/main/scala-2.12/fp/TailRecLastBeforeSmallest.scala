package fp

import scala.annotation.tailrec

/**********************************
 *
 * This is an interview exercise from one of Scala Companies
 *
 **********************************/


object TailRecLastBeforeSmallest {

  /**
   * Find the smallest element of Int List - do not sort, use recurrence
   */
  @tailrec
  def smallest(ls: List[Int], maybeSmallest: Option[Int] = None): Option[Int] = ls match {
    case Nil => maybeSmallest
    case h :: tail => maybeSmallest match {
      case Some(s) => smallest(tail, Some(math.min(s, h)))
      case None => smallest(tail, Some(h))
    }
  }

  /**
   * Find 1 before the smallest element of Int List - do not sort, use recurrence
   */
  @tailrec
  def lastBeforeSmallest(ls: List[Int], beforeSmallestOpt: Option[Int] = None, smallestOpt: Option[Int] = None): Option[Int] = {
    println(s"beforeSmallest ${beforeSmallestOpt} smallest ${smallestOpt} list $ls")
    ls match {
      case Nil => beforeSmallestOpt
      case h :: tail => (beforeSmallestOpt, smallestOpt) match {
        case (None, None) => lastBeforeSmallest(tail, None, Some(h))
        case (None, Some(smallest)) =>
          if (h < smallest)      lastBeforeSmallest(tail, Some(smallest), Some(h))
          else if (h > smallest) lastBeforeSmallest(tail, Some(h), Some(smallest))
          else                   lastBeforeSmallest(tail, None, Some(smallest))
        case (Some(beforeSmallest), Some(smallest)) =>
          if (h < smallest)                            lastBeforeSmallest(tail, Some(smallest), Some(h))
          else if (h > smallest && h < beforeSmallest) lastBeforeSmallest(tail, Some(h), Some(smallest))
          else                                         lastBeforeSmallest(tail, Some(beforeSmallest), Some(smallest))
      }
    }
  }


}
