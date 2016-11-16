package fp

import scala.annotation.tailrec

object ab_IntroToFunProg {

  object PolymorphicFunctions {

    def isSorted[A](as: Array[A], f: (A, A) => Boolean) = {
      @tailrec
      def loop(index: Int, acc: Boolean = true): Boolean =
        if (index + 1 == as.length) acc else loop(index + 1, acc && f(as(index), as(index + 1)))

      if (as.length < 2) true else loop(0)
    }
  }

  object HigherOrderFunctions {

    def curry[A, B, C](f: (A, B) => C): A => (B => C) =
      (a: A) => b => f(a, b)

    def uncurry[A, B, C](f: A => B => C): (A, B) => C =
      (a: A, b: B) => f(a)(b)
  }

}
