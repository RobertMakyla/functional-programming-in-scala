package fp

object af_ApplicativeFunctors {

  /**
   * Functor - each type which has   map(f: A=>B): Functor[B]   and an identity element
   *
   * - map[A, B](fa: F[A])(f: A => B): F[B]
   * - unit[A](a: A): F[A]
   *
   * Functor laws:
   *
   * 1. Associativity:  someFunctor.map(f).map(g) == m.map(x => g(f(x))
   * 2. Identity:       someFunctor.map( identity ) == someFunctor
   */
  trait Functor[A] {
    def map[B](f: A => B): Functor[B]
    def unit(a: A): Functor[A]
  }

  /**
   * Applicative Functors (or Applicatives)
   *
   * Definition 1:
   *
   * - map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
   * - unit[A](a: A): F[A]
   *
   * Definition 2:
   *
   *  - apply[A, C](f: F[A])(f: F[A => C]): F[C]
   *  - unit[A](a: A): F[A]
   */

  trait Applicative1[A] {
    def map2[B, C](fb: Applicative1[B])(f: (A, B) => C): Applicative1[C]
    def unit(a: A): Applicative1[A]
  }

  trait Applicative2[A] {
    def apply[B](f: Applicative2[A => B]): Applicative2[B]
    def unit(a: A): Applicative2[A]
  }

  /**
   * Diferrence between Applicatives and Monads:
   *
   * - Applicatives - has fixed structure of computation (even if some element if None, we still do the rest of computations)
   * - Monad - the result of previous computation has influence weather we continue with computations
   *
   * eg : map2(maybeAddress, maybeLetter)( delivery: (address, letter) => DeliveryRecord): Option[DeliveryRecord]
   *
   * When we cannot get address from maybeAddress, it makes no sense to try to get letter from maybeLetter,
   * Also, suppose that we got address, letter and already have the DeliveryRecord. Why wrap it into Option again ? We shouldn't want that..
   *
   */


}
