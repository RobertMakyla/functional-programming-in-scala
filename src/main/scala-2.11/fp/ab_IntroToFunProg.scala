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

    def compose[A, B, C](f: B => C, g: A => B): A => C =
    // (a: A) => f(g(a))
    // (a: A) => (f compose g)(a)
    // (f compose g)( _ )
      f compose g

    def andThen[A, B, C](f: A => B, g: B => C): A => C =
    // (a: A) => g(f(a))
    // (a: A) => (f andThen g)(a)
    // (f andThen g)( _)
      f andThen g
  }

  object FunctionalDataStructures {
    def tail[T](ls: List[T]): List[T] = ls match {
      case Nil => throw new IllegalArgumentException("list should not be empty")
      case _ :: tail => tail
    }

    def setHead[T](h: T, ls: List[T]): List[T] = h :: ls

    @tailrec
    def drop[T](ls: List[T], n: Int): List[T] = (ls, n) match {
      case (l, i) if l.size < i => throw new IllegalArgumentException("list is too small")
      case (_, 0) => ls
      case (l, i) => drop(tail(l), i - 1)
    }

    @tailrec
    def dropWhile[A](ls: List[A], f: A => Boolean, acc: List[A] = Nil): List[A] = ls match {
      case Nil => acc.reverse
      case h :: tail if f(h) => dropWhile(tail, f, acc)
      case h :: tail if !f(h) => dropWhile(tail, f, h :: acc)
    }

    @tailrec
    def init[A](ls: List[A], acc: List[A] = Nil): List[A] = ls match {
      case Nil => throw new IllegalArgumentException("list should not be empty")
      case _ :: Nil => acc.reverse
      case h :: tail => init(tail, h :: acc)
    }

    /**
     * Compute the length of a list using foldRight
     *
     * def length[A](as: List[A]): Int
     */
    def length[A](as: List[A]): Int = as.foldRight(0)((_, acc) => acc + 1)

    /**
     * Implement foldLeft that is tail recursive
     *
     * def foldLeft[A,B](ls: List[A], z: B)(f: (B, A) => B): B
     */
    @tailrec
    def foldLeft[A, Z](ls: List[A], z: Z)(f: (Z, A) => Z): Z = ls match {
      case Nil => z
      case h :: Nil => f(z, h)
      case h :: tail => foldLeft(tail, f(z, h))(f)
    }

    /**
     * reverse a list using fold
     */
    def reverse[A](ls: List[A]): List[A] = ls.foldLeft(List.empty[A])((acc, elem) => elem :: acc)

    /**
     * foldRight in terms of foldLeft
     *
     *  list(a, b, c)
     *
     *  fold left:                 (z) -> a -> b -> c
     *  fold right via fold left:  (z) -> c -> b -> a
     */
    def foldRight[A, Z](ls: List[A], z: Z)(f: (A, Z) => Z): Z =
      foldLeft(ls.reverse, z)((a, b) => f(b, a))

    /**
     * append in terms of foldRight
     */
    def appendViaFoldRight[A](ls: List[A], z: A): List[A] =
      foldRight(ls, List(z))(_ :: _)

    /**
     * append in terms of foldLeft
     */
    def appendViaFoldLeft[A](ls: List[A], z: A): List[A] =
      foldLeft(ls.reverse, List.empty[A])( (a,b) => b :: a) ++ List(z)

    /**
     * Write a function that concatenates a list of lists into a single list
     */
    def flatten1[A](ls: List[List[A]]): List[A] =
      foldRight(ls, List.empty[A])(_ ++ _)

    def flatten2[A](ls: List[List[A]]): List[A] = ls.flatten

    def flatten3[A](ls: List[List[A]]): List[A] =
      for {
        singleList <- ls
        elem <- singleList
      } yield elem

    def flatten4[A](ls: List[List[A]]): List[A] = ls.flatMap(identity)

    /**
     * implement map using fold:
     *
     * def map[A,B](as: List[A])(f: A => B): List[B]
     */
    def map[A, B](as: List[A])(f: A => B): List[B] = foldRight(as, List.empty[B])((a, acc) => f(a) :: acc)

    /**
     * Write a function filter:
     *
     * def filter[A](as: List[A])(f: A => Boolean): List[A]
     */
    def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, List.empty[A]) {
      (a, acc) => if (f(a)) a :: acc else acc
    }

    /**
     * implement flatMap:
     *
     * def flatMap[A,B](as: List[A])(f: A => List[B]): List[B]
     */
    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = foldRight(as, List.empty[B]) {
      (a, acc) => f(a) ++ acc
    }

    /**
     * Use flatMap to implement filter.
     */
    def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] = flatMap[A, A](as) {
      a => if (f(a)) List(a) else Nil
    }

  }

}
