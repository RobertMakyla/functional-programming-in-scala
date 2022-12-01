package fp

import java.lang.Throwable
import scala.util.Try
import scala.util.control.NonFatal


object ad_MonoidsMonads {

  object Monoids {

    /**
     * In "Category Theory" we have following categories :
     *
     * Magma     - type T and some binary operation (not associative)
     * Semigroup - type T + associative operation
     * Monoid    - type T + associative operation + identity element (zero element)
     * Group     - type T + associative operation + identity element (zero element)
     *             + invertibility (for each element 'a: T' there is another 'b:T' for which op(a,b) == zero element
     */

    /**
     * Monoids
     *
     * - they give possibility to break problem into chunks to use it in parallel programming.
     *
     * foldLeft/foldRight can't be done parallely because it's:  op(a, op(b, op(c, d)))
     *
     * but BALANCED FOLD can compute in parallel,
     * and thanks to monoid laws we are sure that result is the same
     * no matter how it gets split into chunks of computations:
     *
     * op( op(a,b) , op(c,d) )
     */

    /**
     * Monoids consists of 3 things:
     *
     * 1. some type A (can be String, Int, Boolean)
     * 2. an associative binary operation ( + , * , &&)
     *
     * associative operation:  (a op b) op c = a op (b op c)
     *
     * 3. Identity element of type A, applicable anywhere, not bringing any change :
     *
     * a op z = a
     * z op a = a
     *
     * Eg: String, +, ""
     * Eg: List, ++, Nil
     * Eg: Int, *, 1
     * Eg: Int, +, 0
     * EG: X => Monad[Y], flatMap(), unit()   - because one of monadic law is associativity of flatMap
     *
     *      0 + (1 + 2) == (0 + 1) + 2
     *
     *      x => f0(x).flatMap( x => f1(x).flatMap(f2) ) == x => f0(x).flatMap(f1).flatMap(f2)
     *
     *  for {
     *    i <- a
     *    j <- b(i)
     *    k <- c(i, j)
     *  } d (i, j, k)
     *
     *  to to samo co to:
     *
     *  a.flatMap { i =>
     *     b(i).flatMap { j =>
     *        c(i, j).map { k =>
     *           d(i, j, k)
     *        }
     *     }
     *  }
     *
     *	Dzięki temu że monada (Kleisli arrow X=>Monad[Y], flatMap, unit) jest monoidą, możemy skorzystać z associativity
     *	czyli możemy nestować jak chcemy flatMapy w for-comprehension, dekomponując kod na osobne funkcje
     *	które zwracają monadę.. albo wszystko płasko m1.flatMap(f).flatMap(f).flatMap(f) ... jak chcemy ...
     *
     *  To jest największy gain faktu że (X=>Monad[Y], flatMap, unit) jest monoidą..
     *
     *  for {
     *     n      <- m
     *     (i, j) <- for {
     *                  i <- a(n)
     *                  j <- b(i)
     *               } yield (i, j)
     *     k      <- c(i, j)
     *  } yield d(i, j, k)
     *
     *
     *  Bo fakt że dzięki associativity samo składanie funcji można zrownoleglić teoretycznie na 2 wątki
     *  to nie przynosi dużego gain'a (składanie funkcji jest trywialne - to komuptacje zajmują dużo czasu
     *  i zwykle to je chcemy zparallelizować) :
     *
     *      (0+1) + 2 + 3 + (4+5)
     *
     *      (f0.flatMap(f1)).flatMap(f2).flatMap(f3).flatMap( x => f4(x).flatMap(f5)
     *
     *
     *
     * EG: Applicative[T], map2, unit()  - because applicative is associative on map2:
     *
     *                    (ap1.map2(ap2)(f) ).map2(ap3)(f) == ap1.map2( ap2.map2(ap3)(f) )(f)
     */

    trait Monoid[A] {
      def op(a: A, b: A): A

      def zero: A
    }

    /**
     * Semigroup is Monoid but without identity - just type and associative operation
     *
     * so each monoid is semi group (but additionally with identity element)
     */

    trait Semigroup[A] {
      def op(a: A, b: A): A
    }

  }

  object Monads {

    /**
     * Functors - all types that implement .map(f: A => B)  (eg; Option, List, ... )
     */

    /**
     * Monads - type which has 2 functions :
     *
     * - flatMap(f: A => Monad[B]): Monad[B]
     * - unit(a: A): Monad[A]
     *
     * unit() is just wrapping A => Monad[A], but flatMap shows that MONAD is all ABOUT FLOW CONTROL.
     * flatMap defines how to transform M[A] into M[B] having function A=>M[B] just need to get A from M[A]
     */

    trait Monad[A] {
      def flatMap[B](f: A => Monad[B]): Monad[B]
      def unit[B](b: B): Monad[B]
    }

    /** Implement map() using flatmap and unit
     *
     * Prove that a Monad is a functor :)
     *
     * def map[A,B](ls: List[A], f: A=>B): List[B] */

    def map[A, B](ls: List[A], f: A => B): List[B] = {
      def unit[T](t: T): List[T] = List.apply(t)

      ls.flatMap((a: A) => unit(f(a)))
    }


    /** Implement the simplest Monad: Wrapper[A](a: A) */

    case class MyWrapper[A](a: A) extends Monad[A] {
      override def flatMap[B](f: A => Monad[B]): Monad[B] = f(a)
      override def unit[B](b: B): Monad[B] = MyWrapper(b)
      def map[B](f: A => B): Monad[B] = MyWrapper(f(a))
    }

    /** Implement MyOption
     * hint 1: Make the type covariant: MyOption[+A]
     * hint 2: None is a case object extending MyOption[Nothing]
     */
    sealed trait MyOption[+A] {
      def flatMap[B](f: A => MyOption[B]): MyOption[B] = this match {
        case MySome(a) => f(a)
        case MyNone => MyNone
      }

      def unit[B](b: B): MyOption[B] = MySome(b)

      def map[B](f: A => B): MyOption[B] = this match {
        case MySome(a) => unit(f(a))
        case MyNone => MyNone
      }

      def map2[B](f: A => B): MyOption[B] = flatMap((a: A) => unit(f(a)))
    }

    case class MySome[A](a: A) extends MyOption[A]

    case object MyNone extends MyOption[Nothing] // Nothing is a subtype of all types so MyNone will fit any type of MyOption[A] cause MyOption[A] is COVARIANT


    /**
     * hint: use the try{} catch{} block for risk code only in the constructor of companion object
     */
    sealed trait MyTry[A] {
      def flatMap[B](f: A => MyTry[B]): MyTry[B] = this match {
        case MySuccess(v) => f(v)
        case MyFailure(t) => MyFailure(t)
      }

      def unit[B](b: B): MyTry[B] = MySuccess(b)

      def map[B](f: A => B): MyTry[B] = flatMap(a => unit(f(a)))
    }

    case class MySuccess[T](t: T) extends MyTry[T]

    case class MyFailure[T](t: Throwable) extends MyTry[T]

    object MyTry {
      def apply[A](block: => A): MyTry[A] =
        try {
          MySuccess(block)
        } catch {
          case NonFatal(e) => MyFailure(e)
        }
    }

    /*
    hint: when you implement a flatMap, first get all the 'bees' (List[B])
     */

    case class NonEmptyList[A](h: A, tail: List[A]) {
      def flatMap[B](f: A => NonEmptyList[B]): NonEmptyList[B] = {
        val bs: List[B] = all.flatMap(a => f(a).all)
        NonEmptyList(bs.head, bs.tail)
      }

      def unit[B](b: B): NonEmptyList[B] = NonEmptyList(b, Nil)

      private def all: List[A] = h :: tail

      def map[B](f: A => B): NonEmptyList[B] = flatMap(a => unit(f(a))) // useful for the for-comprehension
    }

    object NonEmptyList {
      def apply[A](h: A, tail: A*): NonEmptyList[A] = NonEmptyList(h, tail.toList)
    }
  }
}
