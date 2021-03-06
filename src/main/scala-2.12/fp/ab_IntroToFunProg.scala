package fp

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future, Promise}
import scala.util.{Failure, Success, Try}

object ab_IntroToFunProg {

  object PolymorphicFunctions {

    /*
     * Polymorphic functions work with any type of data (generics)
     */
    def isSorted[A](ls: List[A], f: (A, A) => Boolean): Boolean = ls match {
      case Nil => true
      case singleElem :: Nil => true
      case h :: second :: tail => f(h, second) && isSortedTailRec(second :: tail, f)
    }

    def isSortedTailRec[A](ls: List[A], f: (A, A) => Boolean, acc: Boolean = true): Boolean = ls match {
      case Nil => acc
      case singleElem :: Nil => acc
      case h :: second :: tail => isSortedTailRec(second :: tail, f, acc && f(h, second))
    }
  }

  object HigherOrderFunctions {

    /*
     * Currying - instead of feeding function with all params in a flat way, we can do it partially
     *            and return other function partially specified
     */
    def curry[A, B, C](f: (A, B) => C): A => (B => C) =
      (a: A) => b => f(a, b)
    //(a: A) => f(a, _)

    def uncurry[A, B, C](f: A => B => C): (A, B) => C =
      (a: A, b: B) => f(a)(b)

    /*
     * - composing 2 functions into one function
     * - andThen
     */

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

    /** this function should drop all elements which apply to function f */
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
    def length[A](as: List[A]): Int =
      as.foldRight(0)((_, acc) => acc + 1)

    //as.foldLeft(0)((acc, _) => acc + 1)

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

    // so to implement foldRight in terms of foldLEft we need to:
    // - reverse the list
    // - and change order of arguments in function f

    /**
     * append in terms of foldRight
     */
    def appendViaFoldRight[A](ls: List[A], last: A): List[A] =
      foldRight(ls, List(last))((a: A, acc: List[A]) => a :: acc)

    //foldRight(ls, List(last))(_ :: _)

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

    //List[A] .map(f: A=>B )            : List [B]
    //List[A] .flatMap(f: A=>List[B] )  : List [B]
    def flatten4[A](ls: List[List[A]]): List[A] =
      ls.flatMap((eachSubList: List[A]) => eachSubList)

    //ls.flatMap(_)
    //ls.flatMap(identity)

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
     * using map function from above.
     */
    def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
      case h :: tail => map2(h, sequence(tail))(_ :: _)
      case _ => Some(Nil)
    }

    def sequenceViaForComprehension[A](a: List[Option[A]]): Option[List[A]] = a match {
      case h :: tail =>
        for {
          realHead: A <- h
          realTail: List[A] <- sequence(tail)
        } yield realHead :: realTail
      case _ => Some(Nil)
    }

    /**
     * Implement methods of Either
     */

    case class MyLeft[+L](value: L) extends MyEither[L, Nothing]

    case class MyRight[+R](value: R) extends MyEither[Nothing, R]

    sealed trait MyEither[+L, +R] {

      /*
        This Either is right-biased, which means that Right is assumed to be the default case to operate on.
        If it is Left, operations like map, flatMap, ... return the Left value unchanged:
       */

      /**
       * Implement map(f: R => NewR) applying f only when it's Right
       */
      def map[NewR](f: R => NewR): MyEither[L, NewR] = this match {
        case MyRight(v) => MyRight(f(v))
        case MyLeft(v) => MyLeft(v)
      }

      /**
       * Implement flatMap[NewR](f: L => Either[L, NewR]): Either[L, NewR]
       */
      def flatMap[LL >: L, NewR](f: R => MyEither[LL, NewR]): MyEither[LL, NewR] = this match {
        case MyRight(v) => f(v)
        case MyLeft(v) => MyLeft(v)
      }

      /**
       * Implement orElse[NewR](f: => Either[L, NewR]): Either[L, NewR]
       * applying f only if it's Left
       */
      def orElse[LL >: L, NewR >: R](f: => MyEither[LL, NewR]): MyEither[LL, NewR] = this match {
        case MyRight(v) => MyRight(v)
        case MyLeft(_) => f
      }

      /**
       * Assuming the Either is (is not) right-biased, use it in for-comprehension
       */
      def map2[LL >: L, OtherR, X](that: MyEither[LL, OtherR])(f: (R, OtherR) => X): MyEither[LL, X] = {

        // when Either is not right-biased we have to make projections to use it in for-comprehension:
        //
        // for {
        //    foo <- Right[String,Int](1).right
        //    bar <- Left[String,Int]("nope").right
        // } yield (foo + bar)     // => Left("nope")

        // since we made this Either right-biased, we can simply use it in for-comprehension
        for {
          a <- this // Either[L, R]
          b <- that // Either[L, OtherR]
        } yield f(a, b) // if both either were right I can use all the rights. Otherwise I'll get first left
      }

    }

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

    /** try to de-sugar for-comprehension with 1 line */

    /*
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
     */

    /** try to de-sugar for-comprehension with 2 lines */

    /*
     * -------------------------------------
     *  1 flatMap + 1 map :
     * -------------------------------------
     *
     * for{
     *    someInt     <- Some(1)             // first expression: Monad[Int]
     *    someDouble  <- Some(2.0 * someInt) // flatMap function: Int => Monad[Double]
     * } yield s" I got $doubled"            // map:              Double => String
     *                                       // result:           Monad[String]
     *
     * eg: Some(1).flatMap( i => Some(2.0 * i) ) map ( "I got " + _ )
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

    /**
     * Question: difference between filter() ad withFilter()
     */

    /**
     * Answer:
     *
     * filter() - will take the original collection and produce a new collection
     *
     * withFilter() - will non-strictly (ie. lazily) pass unfiltered values through to later map/flatMap calls,
     * saving another pass through the (filtered) collection, hence it's more efficient.
     *
     * withFilter() -  is specifically designed for working with chains of these methods (map/flatMap),
     * which is what a for comprehension is de-sugared into.
     */
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

      def cons[A](h: => A, t: => MyStream[A]): MyStream[A] = MyCons(() => h, () => t)

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

    /**
     * exists[A](p: A => Boolean): Boolean
     */
    def exists[A](s: MyStream[A], p: A => Boolean): Boolean = s match {
      case MyCons(h, t) => p(h()) || exists(t(), p)
      case MyEmpty => false
    }

    /**
     * def forAll(p: A => Boolean): Boolean
     */
    def forAll[A](s: MyStream[A], p: A => Boolean): Boolean = s match {
      case MyCons(h, tail) => if (!p(h())) false else forAll(tail(), p)
      case MyEmpty => true
    }

    /**
     * Infinite Streams ( ones: 1, 1, 1, 1, 1, 1, 1 ... )
     */
    def ones: MyStream[Int] = MyCons(() => 1, () => ones)

    /**
     * Infinite Streams of any constant value of any type
     */
    def infiniteStreamOf[A](a: A): MyStream[A] = MyCons(() => a, () => infiniteStreamOf(a))

    /**
     * Infinite Streams of natural integers
     */
    def from(n: Int): MyStream[Int] = MyCons(() => n, () => from(n + 1))

    /**
     * Infinite Streams of fibonacci numbers
     */
    def fibs(lastMinusTwo: Int = 0, lastMinusOne: Int = 1, last: Int = 1): MyStream[Int] =
      MyCons(() => lastMinusTwo, () => fibs(lastMinusOne, last, last + lastMinusOne))

  }

  object PurelyFunctionalState {

    /**
     * scala.util.Random - this one has sime state which changes because we cannot predict next output.
     * So it's methods are not referentially transparent - hard to test, predict what will happen.
     */

    /**
     * Purely functional Random Number Generator (RNG)
     *
     * To make it functional, method should return not only random result but also mutated generator.
     * This way it's predictable, testable.
     */

    trait RNG {
      def int: (Int, RNG)
    }

    case class SimpleRNG(seed: Long) extends RNG {
      override def int: (Int, RNG) = {
        val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
        val nextRNG = SimpleRNG(newSeed)
        val n = (newSeed >>> 16).toInt //random number is generated based on seed only.
        // so MySimpleRNG.nextInt will give the same result all the time - cause the seed is the same.

        // >>> przesunięcie newSeed w prawo o 16 miejsc (z lewej dodając zera)

        (n, nextRNG)
      }
    }

    /**
     * Write a function to generate a list of random integers.
     */
    def ints(count: Int)(rng: RNG): (List[Int], RNG) = if (count == 0) (Nil, rng)
    else {
      val (i, rng1) = rng.int
      val (is, rng2) = ints(count - 1)(rng1)
      (i :: is, rng2)
    }

    /**
     * Think of a new type that would cover passing mutated RNG all the time... :
     */
    type Rand[+A] = RNG => (A, RNG) // to not pass mutated RNG explicitly all the time..

    /**
     * Implement def unit[A](a: A): Rand[A]
     */
    def unit[A](a: A): Rand[A] = (a, _)

    /**
     * Implement def map[A, B](r: Rand[A])(f: A => B): Rand[B]
     */
    def map[A, B](r: Rand[A])(f: A => B): Rand[B] = (rng: RNG) => {
      val (a: A, rng2: RNG) = r(rng)
      (f(a), rng2)
    }

    /**
     * Implement def flatMap[A,B](r: Rand[A])(g: A => Rand[B]): Rand[B]
     */
    def flatMap[A, B](r: Rand[A])(g: A => Rand[B]): Rand[B] = (rng: RNG) => {
      val (a: A, rng2: RNG) = r(rng)
      val randB: Rand[B] = g(a)
      val (b: B, rng3: RNG) = randB(rng2)
      (b, rng3)
    }

    /**
     * What we have just written (unit, map, flatMap) are functions for working with state actions.
     *
     * Now we can start playing with it as if Rand was a monad:
     *
     * val randoms: Rand[(Int, Int, Int)] = for {   //in other words: RNG => ((Int, Int, Int), RNG)
     *   a <- int // RNG => (A, RNG)
     *   b <- int
     *   c <- int
     * } yield (a, b, c)
     *
     */
  }

  object Futures { // source: http://docs.scala-lang.org/overviews/core/futures.html

    def takeYourTime(calculationLabel: String) = for (i <- 1 to 3) {
      print(s"$calculationLabel : $i\n"); Thread.sleep(100)
    }

    case class Name(value: String)
    case class UpperCaseName(value: String)
    case class UpperCaseNameWithSpaces(value: String)

    // long lasting operation
    def makeUpperCase(name: Name): UpperCaseName = {
      takeYourTime(s"Making upper case of $name")
      UpperCaseName(name.value.toUpperCase)
    }

    // long lasting operation
    def addSpaces(upperCaseName: UpperCaseName): UpperCaseNameWithSpaces = {
      takeYourTime(s"Adding spaces to $upperCaseName")
      UpperCaseNameWithSpaces(upperCaseName.value.mkString(" "))
    }

    /**
     * Execution Context
     *
     * It is possible to execute Future's computations in new Thread, Thread pool,
     * or current thread (discouraged cause it's supposed to be in parallel)
     *
     * Futures and Promises always do computations in Execution Context
     *
     * scala.concurrent.ExecutionContext.global - that's global static THREAD POOL
     *
     * the number of threads is number of processors (4):
     *    Runtime.getRuntime().availableProcessors()
     */

    val globalEC: ExecutionContextExecutor = scala.concurrent.ExecutionContext.global

    def newAsyncFuture(ec: ExecutionContext) = Future {
      makeUpperCase(Name("async_future"))
    }(ec) //not needed when ec is implicit

    //makeUpperCase - might be already calculating - we can't be sure

    /**
     * Future(...) calls Future.apply() which creates and schedules asynchronous computation in thread pool.
     *
     * This will calculate parallely in separate thread from global thread pool
     * But we don't know when !!!
     *
     * We can stop and wait until it's completed: Await.ready(firstFuture, 10 seconds)
     * We can stop and wait for the result:       Await.result[T](firstFuture, 10 seconds): T
     */

    /**
     * failing Future
     *
     * As it's executed in different thread) encapsulates exceptions just like Try( )
     */
    def newFailingFuture(ec: ExecutionContext): Future[String] = Future(throw new RuntimeException("boom"))(ec)

    /**
     * Callback
     *
     * We have them to process result of the future after it's completed.
     * We don't need to wait for a future to complete with blocking Await.result() / Await.ready().
     *
     * Callback methods are called asynchronously once the future is complete.
     **
     * onComplete[U](f: Try[T] => U): Unit
     * onSuccess[U](f: PartialFunction[T, U] ): Unit       - Deprecated, use only when we want action after success
     * onFailure[U](f: PartialFunction[Throwable, U]): Unit - Deprecated, use only when we want action after failure
     */

    /**
     * Callback - execution timing
     *
     * Since callback methods need future result, they are executed after future is completed.
     * However not necessarily by the same thread.
     *
     * E.g.: future is completed, then there's no free thread in pool,
     * then EVENTUALLY callback is executed in different free thread.
     */

    /**
     * Callback - execution order
     *
     * They always return: Unit (invocations cannot be chained one after another)
     *
     * We can register many different (or the same) callbacks on a single future.
     * But the order of execution CANNOT be defined:
     *  - it can be executed one after another
     *  - or parallely in different threads from the pool
     *
     * That's why they should never modify anything - there should be no side effects.
     *
     * muFuture onComplete  (...)
     * muFuture onSuccess   ( /* do sth 1*/)
     * muFuture onSuccess   ( /* do sth 2*/)
     * muFuture onComplete  ( /* do yet another thing*/)
     * muFuture onFailure   ( /* log the error */)
     */

    /**
     * Callback - execution conditions
     *
     * Even if some callbacks throw Exception, other are still eventually executed regardless !!!
     * cause it's all in different threads, and always return Unit anyway...
     *
     * Callbacks are executed as long as future is Success/Failure, and type in PartialFunctions matches.
     *
     * eg: onFailure{ t:Throwable     => /* executed at any failure*/ }
     * eg: onFailure{ t:SomeException => /* executed only at SomeException*/ }
     *
     * If some callbacks never complete (e.g. the callback contains an infinite loop),
     * the other callbacks may not get thread to be executed at all.
     *
     * In these cases, a potentially blocking callback must use the blocking{} :
     *
     * Future {
     *   blocking {  // - thanks to some default max waiting time, is can avoid deadlock.
     *      // some blocking stuff
     *   }
     * }
     */

    val aFuture: Future[UpperCaseName] = newAsyncFuture(globalEC)

    aFuture.onComplete {
      case Success(result: UpperCaseName) => println(s"log 1: future finished with $result")
      case Failure(t) => println(s"Error: ${t.getMessage}")
    }(globalEC)

    aFuture.onSuccess {
      case result: UpperCaseName =>
        println(s"log 2: future finished with $result")
        throw new RuntimeException("handling success has crashed, but it doesn't affect other callbacks")
    }(globalEC)

    aFuture.onFailure {
      case t: Throwable => println(s"Error: ${t.getMessage}")
    }(globalEC)

    /**
     * Composing Futures - to be executed synchronously (map/flatMap)
     *
     * First it completes makeUpperCase: Name => UpperName
     * Then, it starts addSpaces: UpperName => UpperNameWithSpaces
     *
     * If we gave different execution context to map, then thread pool would be different
     * and so synchronous execution would not be guaranteed !!!!
     */

    def newSyncFuture(ec: ExecutionContext): Future[UpperCaseNameWithSpaces] = Future {
      makeUpperCase(Name("sync_future"))
    }(ec).map {
      upper => addSpaces(upper)
    }(ec)

    /**
     * For-Comprehension (another way of executing synchronously)
     *
     * If any of futures throws an exception, then it stops mapping/flatMapping and returns Failure with first exception ..
     */
    def futureForComprehension(ec: ExecutionContext): Future[String] = {
      implicit val innerEC = ec
      for {
        a <- Future("Robert ")
        b <- Future(a * 3)
        c <- Future(b.toUpperCase)
      } yield c
    }

    /**
     * foreach() - it's the same as onSuccess()  so just registering asynchronous action in case of success
     *
     * Functions that map/flatMap in case of success (if failure then result is the same):
     *
     *   map[U](     pf: PartialFunction[T, U]         ): Future[U]
     *   flatMap[U]( pf: PartialFunction[T, Future[U] ]): Future[U]
     *
     * Functions that map/flatMap in case of failure (if success then result is the same):
     *
     *   recover[U](     pf: PartialFunction[Throwable, U]         ): Future[U]
     *   recoverWith[U]( pf: PartialFunction[Throwable, Future[U] ]): Future[U]
     */

    /**
     * Function that gives another shoot for another future to be successful is:
     *
     *     Future[U].fallbackTo(second: Future[U]): Future[U]
     *
     * If first future fails and the second one is successful, then second one will be returned.
     * If both fail, the failure of the first one will be returned.
     */

    /**
     * Function called after future's completed (successfully or not),
     * and still returns this future (so can be chained):
     *
     *     Future[U].andThen(pf: PartialFunction[Try[U], Any]): Future[U]
     *
     * eg:  Future { /* ... */}
     *       .andThen{ case Success(sth) => log.info(sth)}
     *       .andThen{ case Failure(t) => log.error(t)}
     *
     *  But we don't really need this chaining it cause we can register many callback methods on future
     *  (onComplete: Unit , onSuccess: Unit , onFailure: Unit )
     */

    /**
     * Projection Future[U].failed  - inverting result if we care about future failure
     *
     * If future fails, then it returns Success(Throwable)
     * If future succeeds, then failed returns Failure(NoSuchElementException)
     */

    def futureFailed(ec: ExecutionContext) = newFailingFuture(ec).failed.onComplete {
      case Success(t: Throwable) => println("ok, we got failure as expected")
      case Failure(nse: NoSuchElementException) => println("somwthing went wrong")
    }(ec)

    /**
     * Promises - write-once container which completes a future.
     *
     * Promise.future           - returning the future of this promise (but it's not completing future)
     *
     * Promise.success(t: T): Promise[T]               - COMPLETING future with success !
     * Promise.failure(t: Throwable): Promise[T]       - COMPLETING future with failure !
     * Promise.complete(t: Try[T]): Promise[T]         - COMPLETING future with success or failure, depending on argument
     * Promise.completeWith(f: Future[T]): Promise[T]  - COMPLETING future with success or failure, depending on argument
     *
     * WARNING: if either 2 of these functions are called, it will throw IllegalStateException
     *          because we can/should complete Promise only once
     *
     *
     *  safer but non-deterministic:
     *
     * Promise.trySuccess(t: T): Boolean          - COMPLETING  if it's not complete yet, returns Boolean if it was complete
     * Promise.tryCailure(t: Throwable): Boolean  - COMPLETING  if it's not complete yet, returns Boolean if it was complete
     * Promise.tryComplete(t: Try[T]): Boolean    - COMPLETING  if it's not complete yet, returns Boolean if it was complete
     */

    def newFutureFromPromise[T](someValue: T)(shouldPass: Boolean): Future[T] = {

      val p = Promise[T]()
      val f = p.future

      println(s"at first, Promise.future is ${if (f.isCompleted) "" else "NOT"} completed ... ")

      if (shouldPass) //but abut the result we can decide long after future is complete.
        p success someValue
      else
        p failure new RuntimeException("you said it should fail")

      println(s"but after using success() or failure(), Promise's future ${if (f.isCompleted) "gets" else "still NOT"} completed ... ")

      f
    }

    /**
     * Sequence of Futures - asynchronous (parallel) execution of many futures
     *
     * Future.sequence(s: Seq[Future[T]]): Future[Seq[T]] completes when either:
     * - all the futures have completed successfully, then in onComplete it matches Success(sequenceOfValues)
     * - one of the futures has failed, then OnComplete it goes to Failure(firstThrowable) and stops waiting for other futures
     */

    def sequenceOfFutures(ec: ExecutionContext) = {
      implicit val ecImplicit = ec

      val manyfutures: Seq[Future[UpperCaseName]] = Seq(newAsyncFuture(ec), newAsyncFuture(ec))

      Future.sequence(manyfutures).onComplete {
        case Success(upperCaseNames: Seq[UpperCaseName]) => println("each future completed successfully")
        case Failure(t: Throwable) => println("some future has failed with " + t.getMessage)
      }

    }
  }

}
