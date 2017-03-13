package fp

import scala.annotation.tailrec
import scala.util.{Try, Right, Left, Either}
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
     * list(a, b, c)
     *
     * fold left:                 (z) -> a -> b -> c
     * fold right via fold left:  (z) -> c -> b -> a
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
      foldLeft(ls.reverse, List.empty[A])((a, b) => b :: a) ++ List(z)

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

    /**
     * zip adding Int's
     */
    def zip(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
      case (Nil, _) => b
      case (_, Nil) => a
      case (aHead :: aTail, bHead :: bTail) => aHead + bHead :: zip(aTail, bTail)
    }

    /**
     * Generalize zip to take any type and zipping function:
     *
     * zipWith
     */
    def zipWith[A](a: List[A], b: List[A])(f: (A, A) => A): List[A] = (a, b) match {
      case (Nil, _) => b
      case (_, Nil) => a
      case (aHead :: aTail, bHead :: bTail) => f(aHead, bHead) :: zipWith(aTail, bTail)(f)
    }

    /* ReduceLeft / ScanLeft :

     List("A", "B", "C").foldLeft("z")(_ + _)  // "zABC"
     List(1, 2, 3).foldLeft(0)(_ - _)      // 0 - 1 - 2 - 3

     List("A", "B", "C").reduceLeft(_ + _)   // "ABC"
     List(1, 2, 3).reduceLeft(_ - _)     // 1 - 2 - 3

       - so it's like fold but without starting value

     List("A", "B", "C").scanLeft("z")(_ + _)  // List("z", "zA", "zAB", "zABC")
     List(1, 2, 3).scanLeft(0)(_ - _)         // List(0, 0-1, 0-1-2, 0-1-2-3)

       - so it returns each step of folding as a list

    */

    /**
     * implement hasSubsequence for checking whether a List contains another List as a subsequence
     *
     * def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean
     */
    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
      val sublistSize = sub.size
      if (sup.size < sublistSize) false
      else if (sup.size == sublistSize) sup == sub
      else {
        val toCheck = sup.take(sublistSize)
        toCheck == sub || hasSubsequence(sup.tail, sub)
      }

    }

    /*
     * TREE
     */
    sealed trait Tree[+A]

    case class Leaf[A](value: A) extends Tree[A]

    case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

    /** Write a function size that counts the number of nodes (leaves and branches) in a tree */
    def size[A](t: Tree[A]): Int = t match {
      case Branch(l, r) => size(l) + size(r) + 1
      case Leaf(_) => 1
    }

    /** Write a function maximum that returns the maximum element in a Tree[Int] */
    def max(t: Tree[Int]): Int = t match {
      case Branch(l, r) => max(l) max max(r)
      case Leaf(v) => v
    }

    /** Write a function depth that returns the maximum path length from the root of a tree to any leaf */
    def depth[A](t: Tree[A]): Int = t match {
      case Branch(l, r) => 1 + (depth(l) max depth(r))
      case Leaf(_) => 1
    }

    /** Write a function map , analogous to the method of the same name on List ,
     * that modifies each element in a tree with a given function */
    def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
      case Leaf(a) => Leaf(f(a))
    }

    /** Folding a TREE !!!
     *
     * As tree is 2d structure (branches next to each other and branches containing other branches)
     * we need 2 functions to fold it :
     *
     * - f: for mapping value of each leaf
     * - g: for merging left and right piece of branch into one value
     * */
    def fold[A, Z](tree: Tree[A])(f: A => Z)(g: (Z, Z) => Z): Z = tree match {
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
      case Leaf(a) => f(a)
    }

    def sizeViaFold[A](t: Tree[A]): Int =
      fold(t)(_ => 1)(_ + _ + 1) // leftSize + rightSize + 1

    def maxViaFold(t: Tree[Int]): Int =
      fold(t)(identity)(_ max _) // leftMax max rightMax

    def depthViaFold[A](t: Tree[A]): Int =
      fold(t)(_ => 1)((ld, rd) => (ld max rd) + 1) // (leftDepth max rightDepth) + 1

    def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
      fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _)) // Branch(leftMapped, rightMapped)

  }

  object HandlingErrorsWithoutExceptions {

    /*
     * Exceptions break Referential Transparency
     *
     * -- because they make result unexpected.
     *    eg: func:Int=>Int may not return Int but throw an exception
     *
     * Exceptions should be used only for necessary error handling (IO/Network) and not flow control, because:
     *
     *  -- they're not type-safe (less control over returned type)
     *
     *     (Option/Either are type safe - so we have control)
     *
     *  -- they don't work with higher-order functions
     *     eg. map(func: A=>B) expects func not to throw anything because there's no mechanism
     *         how to behave if for 1 element an exception is thrown
     *
     *     (Option/Either work fine with higher order functions)
     */

    sealed trait MyOption[+A] {
      def map[B](f: A => B): MyOption[B] = this match {
        case MySome(a) => MySome(f(a))
        case MyNone => MyNone
      }

      def flatMap[B](f: A => MyOption[B]): MyOption[B] = this match {
        case MySome(a) => f(a)
        case MyNone => MyNone
      }

      /** Convert Some to None if the value doesn’t satisfy f */
      def filter(f: A => Boolean): MyOption[A] = this match {
        case MySome(a) if f(a) => MySome(a)
        case _ => MyNone
      }

      def getOrElse[B >: A](default: => B): B = this match {
        case MySome(a) => a
        case MyNone => default
      }

      def orElse[B >: A](ob: => MyOption[B]): MyOption[B] = this match {
        case MySome(a) => this
        case MyNone => ob
      }
    }

    case class MySome[A](get: A) extends MyOption[A]

    case object MyNone extends MyOption[Nothing]

    /**
     * Write a generic function map2 that combines two Option values using a function.
     * If either Option value is None , then the return value is too.
     */
    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
      case (Some(realA: A), Some(realB: B)) => Some(f(realA, realB))
      case _ => None
    }

    // using for-comprehension :)
    def map2ViaForComprehension[A, B, C](maybeA: Option[A], maybeB: Option[B])(f: (A, B) => C): Option[C] = for {
      a <- maybeA
      b <- maybeB
    } yield f(a, b)

    /*
     * Monad Transforming
     */

    /**
     * Transform a list of Options into Option of List
     */
    def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
      case h :: tail => map2(h, sequence(tail))(_ :: _)
      case _ => Some(Nil)
    }

    def sequenceViaForComprehension[A](a: List[Option[A]]): Option[List[A]] = a match {
      case h :: tail =>
        for {
          realHeal: A <- h
          realTail: List[A] <- sequence(tail)
        } yield realHeal :: realTail
      case _ => Some(Nil)
    }

    /**
     * Implement methods of Either
     */
    sealed trait MyEither[+L, +R] {
      def map[RR](f: R => RR): MyEither[L, RR] = this match {
        case MyRight(v) => MyRight(f(v))
        case MyLeft(v) => MyLeft(v)
      }

      def flatMap[LL >: L, B](f: R => MyEither[LL, B]): MyEither[LL, B] = this match {
        case MyRight(v) => f(v)
        case MyLeft(v) => MyLeft(v)
      }

      def orElse[LL >: L, RR >: R](b: => MyEither[LL, RR]): MyEither[LL, RR] = this match {
        case MyRight(v) => MyRight(v)
        case MyLeft(_) => b
      }

      def map2[LL >: L, R2, X](that: MyEither[LL, R2])(f: (R, R2) => X): MyEither[LL, X] =
        for {
          a <- this
          b <- that
        } yield f(a, b)
    }

    case class MyLeft[+L](value: L) extends MyEither[L, Nothing]

    case class MyRight[+R](value: R) extends MyEither[Nothing, R]

    /**
     * Implement sequence for Either.
     * It should return the first error that’s encountered, if there is one.
     */

    def sequence[E, A](es: List[MyEither[E, A]]): MyEither[E, List[A]] = es match {
      case h :: tail => h.map2(sequence(tail))(_ :: _)
      case _ => MyRight(Nil)
    }

    def sequenceViaForComprehension[E, A](es: List[MyEither[E, A]]): MyEither[E, List[A]] = es match {
      case h :: tail =>
        for {
          realH <- h
          realTail <- sequence(tail)
        } yield realH :: realTail
      case _ => MyRight(Nil)
    }

  }

  object forComprehensionUnderTheHood {
    /**
     * -------------------------------------
     *  1 map :
     * -------------------------------------
     *
     * for{
     *    a <- monadOfA          // first expression: Monad[A]
     * } yield a2b(a)            // map:              A => B
     *                           // result:           Monad[B]
     *
     * eg: Some(123).map( i => "" + i )
     *
     * -------------------------------------
     *  1 flatMap + 1 map :
     * -------------------------------------
     *
     * for{
     *    a <- monadOfA           // first expression: Monad[A]
     *    b <- fromAtoMonadOfB    // flatMap function: A => Monad[B]
     * } yield b2c(b)             // map:              B => C
     *                            // result:           Monad[C]
     *
     * eg: Some(2).flatMap( i => Try{ "Hello"(i) }.toOption ) map ( c => c.toUpper)
     *
     */
    def isForComprehensionOptionWorking = {
      def maybeIndex: Option[Int] = Some(0)
      def maybeCharFrom(index: Int, s: String): Option[Char] =
        Try {s(index)}.toOption

      //for comprehension
      val result1: Option[Char] = for {
        i <- maybeIndex
        c <- maybeCharFrom(i, "robert")
      } yield c.toUpper

      // de-sugared
      val result2: Option[Char] = maybeIndex.flatMap((i: Int) => maybeCharFrom(i, "robert")).map(_.toUpper)

      result1 == result2
    }

    /**
     * Since Scala 2.12 Either has right-biased /bajest/ flatMap,
     * so we can use it in for-comprehension
     */
    def isForComprehensionEitherWorking = {
      def maybeIndex: Either[Throwable, Int] = Right(0)
      def maybeCharFrom(index: Int, s: String): Either[Throwable, Char] =
        Try {s(index)}.toEither

      //for comprehension
      val result1 = for {
        i <- maybeIndex
        c <- maybeCharFrom(i, "robert")
      } yield c toUpper

      // de-sugared
      val result2 = maybeIndex.flatMap((i: Int) => maybeCharFrom(i, "robert")).map(_.toUpper)

      //println{s" is $result1 == $result2 ?"}
      result1 == result2
    }

  }

  object Streams {

    /**
     * Call-by-name argument makes functions NON-STRICT.
     * Call-by-value argument makes functions STRICT.
     *
     * call-by-name argument is not evaluated until it's passed to first STRICT function.
     */

    //STREAMS
    sealed trait MyStream[+A]

    case object MyEmpty extends MyStream[Nothing]

    case class MyCons[+A](h: () => A, t: () => MyStream[A]) extends MyStream[A]

    object MyStream {

      def cons[A](hd: => A, tl: => MyStream[A]): MyStream[A] = {
        lazy val head = hd
        lazy val tail = tl
        MyCons(() => head, () => tail)
      }

      def empty[A]: MyStream[A] = MyEmpty

      def apply[A](as: A*): MyStream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
    }

    /**
     * Write a function to convert a Stream to a List
     */
    def toList[A](s: MyStream[A]): List[A] = s match {
      case MyCons(h, t) => h() :: toList(t())
      case MyEmpty => Nil
    }

    @tailrec
    def toListTailRec[A](s: MyStream[A], acc: List[A] = Nil): List[A] = s match {
      case MyCons(h, t) => toListTailRec(t(), h() :: acc)
      case MyEmpty => acc.reverse
    }

    /**
     * Write a function take(n)
     */
    def take[A](s: MyStream[A], n: Int): List[A] = s match {
      case MyCons(h, t) if n != 0 => h() :: take(t(), n - 1)
      case MyCons(h, t) if n == 0 => Nil
      case MyEmpty => Nil
    }

    /**
     * def takeWhile(p: A => Boolean): Stream[A]
     */
    def takeWhile[A](s: MyStream[A], p: A => Boolean): MyStream[A] = s match {
      case MyCons(h, t) if p(h()) => MyCons(() => h(), () => takeWhile(t(), p))
      case MyCons(h, t) if ! p(h()) => MyEmpty
      case MyEmpty => MyEmpty
    }
  }

}
