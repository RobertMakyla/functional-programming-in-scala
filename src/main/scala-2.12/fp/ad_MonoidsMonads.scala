package fp

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
     *	Dzięki temu że (Kleisli arrow X=>Monad[Y], flatMap, unit) jest monoidą, możemy skorzystać z associativity
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
    val intMultiplication = new Monoid[Int]{
      override def op(a: Int, b: Int): Int = a * b

      override def zero: Int = 1
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
      def unit(a: A): Monad[A]
    }

    /**
     * Q: Since Monad needs only 2 functions by definition, where do we get map() from ?
     * Isn't it required eg. in for-comprehension ?
     *
     * A: map() can be implemented with flatMap and unit:
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

    /**
     * Definition 2 (from red book)
     *
     * Monad can also be type which has unit() and compose() functions, and satisfies monadic laws.
     * But still compose can be easily implemented in terms of flatMap.
     * So definition with unit() and flatMap() seems minimal.
     */

    /**
     * Q: Implement compose(kleisli1, kleisli2) in terms of flatMap(kleisli)
     */
    trait Monad3[A] {
      def flatMap[B](f: A => Monad3[B]): Monad3[B]
      def unit(a: => A): Monad3[A]

      def compose[B,C](k1: A => Monad3[B], k2: B => Monad3[C]): A => Monad3[C] =
        (a:A) => k1(a).flatMap(k2)
    }

    /** Implement the simplest Monad: Wrapper[A](a: A) */

    case class MyWrapper[A](a: A) extends Monad[A] {
      override def flatMap[B](f: (A) => Monad[B]): Monad[B] = f(a)
      override def unit(a: A): Monad[A] = MyWrapper(a)

      def map[B](f: A=>B): MyWrapper[B] = MyWrapper(f(a)) //useful in for-comprehension
    }



  }

}
