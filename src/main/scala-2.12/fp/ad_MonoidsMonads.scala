package fp

import fp.ab_IntroToFunProg.HandlingErrorsWithoutExceptions.MySome

object ad_MonoidsMonads {

  object Monoids {

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
     */

    trait Monoid[A] {
      def op(a: A, b: A): A

      def zero: A
    }

    /**
     * Implement monoid: String, +, ""
     */
    val stringConcat = new Monoid[String] {
      override def op(a: String, b: String): String = a + b

      override def zero: String = ""
    }

    /**
     * Implement monoid: Int, *, 1
     */
    val intMultiplication = new Monoid[Int] {
      def op(a1: Int, a2: Int) = a1 * a2
      val zero = 1
    }

    /**
     * Ex 10.3
     *
     * A function having the same argument and return type is sometimes called an
     * endofunction. Write a monoid for endofunctions:
     *
     * (type: A=>A, operation: ??? , zero element: ??? )
     */
    def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
      def op(a1: A => A, a2: A => A): A => A = a1 compose a2

      def zero: A => A = identity // (a => a)
    }

    /** Ex 10.4 use property-based testing to test monoid laws */

    /**
     * Folding lists with Monoids :
     *
     * Because of associativity and identity element laws,
     * fold left and fold right must give the same result
     */

    val words = List("a", "b", "c")

    val foldedWords1 = words.foldLeft(stringConcat.zero)(stringConcat.op) // abc

    val foldedWords2 = words.foldRight(stringConcat.zero)(stringConcat.op) // abc


    /**
     * Ex 10.5
     * If our type doesn't fit to any Monoid, we need to map it to some other type which does.
     *
     * Implement foldMap:
     *
     * def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B
     */

    def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
      as.map(f).foldLeft(m.zero)(m.op)

    /**
     * Monoids are composable
     * if A and B are monoids, then tuple (A, B) is also a monoid (called their product)
     *
     * Implement def productMonoid[A,B](a: Monoid[A], b: Monoid[B]): Monoid[(A,B)]
     */

    def productMonoid[A, B](a: Monoid[A], b: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
      override def op(t1: (A, B), t2: (A, B)): (A, B) = (a.op(t1._1, t2._1), b.op(t1._2, t2._2))
      override def zero: (A, B) = (a.zero, b.zero)
    }

  }

  object Monads {

    /**
     * Functors - all types that implement .map(f: A => B)  (eg; Option, List, ... )
     */

    /**
     * Monads - type which have 2 functions :
     *
     * - flatMap(f: A => Monad[B]): Monad[B]
     * - unit(a: A): Monad[A]
     */

    trait Monad[A] {
      def flatMap[B](f: A => Monad[B]): Monad[B]
      def unit(a: => A): Monad[A]
    }

    /**
     * Since Monad needs only 2 functions by definition, where do we get map() from ?
     * Isn't it required eg. in for-comprehension ?
     * Answer: map() can be implemented with flatMap and unit:
     */

    /** Implement map() using flatmap and unit
     *
     * def map[A,B](ls: List[A], f: A=>B): List[B] */

    def map[A, B](ls: List[A], f: A => B): List[B] = {
      def unit[T](t: T): List[T] = List.apply(t)
      ls.flatMap(a => unit(f(a)))
    }

    /**
     * Q: Is Monad[T] a Functor[F] ?
     *
     * A: Yes, because each Monad has flatMap and unit
     *    so it can implement map() - the only function required for Functor
     */


  }

}
