package fp

object af_ApplicativeFunctors {

  /**
   * Functor - each type which has   map(f: A=>B): Functor[B]   and an identity element
   *
   * - map[A, C](fa: F[C])(f: A => C): F[C]
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
   * In Functors, our value was wrapped in a level of abstraction (Option, List, .. etc)
   * but Applicative takes it to next level and wraps also the function:
   *
   * Definition 1:
   *
   *  - apply[A, C](f: F[A])(f: F[A => C]): F[C]
   *  - unit[A](a: A): F[A]
   *
   * Definition 2:
   *
   * - map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
   * - unit[A](a: A): F[A]
   *
   */

  trait Applicative2[A] {
    def apply[B](f: Applicative2[A => B]): Applicative2[B]
    def unit[T](a: T): Applicative2[T]

    /** All Applicatives are Functors ... Prove it: */
    def map[B](f: A => B): Applicative2[B] = apply(unit(f))
  }

  trait Applicative1[A] {
    def map2[B, C](fb: Applicative1[B])(f: (A, B) => C): Applicative1[C]
    def unit[T](a: T): Applicative1[T]

    /**
     * Implement apply in terms of map2
     */
    def apply2[C](f: Applicative1[A => C]): Applicative1[C] =
      map2(f: Applicative1[A => C])((a: A, a2c: A => C) => a2c(a))

    /** All Applicatives are Functors ... Prove it: */
    def map[C](f: A => C): Applicative1[C] =
      map2(unit(f): Applicative1[A => C])((a: A, a2c: A => C) => a2c(a))

    /** or simpler */
    def another_map[C](f: A => C): Applicative1[C] =
      map2(unit( () ): Applicative1[Unit])((a: A, _) => f(a) )

  }

  /**
   * Diferrence between Applicatives and Monads:
   *
   * - Monad - the result of previous computation has influence weather we continue with computations
   *   (context sensitive computations)
   *
   * - Applicatives - has fixed structure of computation (even if some element if None, we still do the rest of computations)
   *   (context-free computations)
   *
   * eg : map2(maybeAddress, maybeLetter)( deliver: (address, letter) => DeliveryRecord): Option[DeliveryRecord]
   *
   * When we cannot get address from maybeAddress, it makes no sense to try to get letter from maybeLetter,
   * Also, suppose that we got address, letter and already have the DeliveryRecord.
   * Why wrap it into Option again ? We shouldn't want that..
   */


  /**
   * When to use Monads
   *
   * - Monad - are better when results depend on each other (but not necessarily)
   *           and we want to avoid unnecessary computations in case of failure
   *
   * eg 1 : flow when next result depends on first one - we want Monad here)
   *
   *  for {
   *      address <- maybeAddress
   *      letterToSend <- maybeLetterTo( address )
   *  } yield letterToSend
   *
   * eg 2 : flow when next result doesn't depend on first one
   *        but we still don't want to continue computations after first failure, so still Monads are better her
   *
   *  for {
   *      address <- maybeAddress
   *      letter <- maybeLetter
   *  } yield deliver(address, letter)
   */


  /**
   * When to use Applicatives
   *
   * - Applicatives - are better when we don't want to stop the chain of computations in case of failure.
   *                  If we want just independent wrapping of all values (and get info about all the errors, not just first)
   *                  All failed computations are accumulated into one final result, which carries full, combined error.
   *
   * eg 1: the Validation applicative - often called “a gateway drug to Scalaz”.
   *
   *  val config = (validateName(name) |@| validateAge(age) |@| validateEmail(email))(Config.apply)
   *
   * eg 2: here, we would still want to try to get p2 even if p1 failed - Applicative is better here)
   *
   *  for {
   *      p1 <- maybeParam1FromConfig
   *      p2 <- maybeParam2FromConfig
   *  } yield Config(p1, p2)           // EEERKKKK !! change me to Validation Applicative from ScalaZ :)
   *
   */


}
