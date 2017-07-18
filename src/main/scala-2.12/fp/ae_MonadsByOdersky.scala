package fp

object ae_MonadsByOdersky extends App {

  println {
    """Monad must satisfy 3 laws:
1. Associativity          :  (m flatMap f1) flatMap f2 = m flatMap( x => f1(x) flatMap f2 )   // it's like (1+2)+3=1+(2+3)
2. Left identity (unity)  :  unit(x) flatMap f = f(x)
3. Right identity (unity) :  m flatMap unit = m
    """
  }

  /** Q: Implement Option / List Monad */

  //////////////////////////////////////////////////////////////////////
  //
  //  Option[T] :
  //
  //  abstract class Option[T] {
  //    def flatMap[U](f: T => Option[U]): Option[U] = this match {
  //      case Some(x) => f(x)
  //      case None => None[U]
  //    }
  //    def unit(t: T) = Some(t)
  //  }
  //
  //////////////////////////////////////////////////////////////////////


  val maybeOne: Option[Int] = Some(1)

  def timesHundred(i: Int): Option[Int] = Some(i * 100)

  def timesTwo(i: Int): Option[Int] = None //or: Some(i * 2)

  def unit(i: Int): Option[Int] = Some(i)

  println {
    s"""Monad laws for Option[T]:
1. Associativity         :  ${(maybeOne flatMap timesTwo flatMap timesHundred) == maybeOne.flatMap(one => timesTwo(one) flatMap timesHundred)}
2. Left identity (unity) :  ${(unit(1) flatMap timesHundred) == timesHundred(1)}
3. Right identity (unity):  ${(maybeOne flatMap unit) == maybeOne}
"""
  }

  /** Q: Implement both flatMap() and unit() of Try { } - is it a monad ? */

  ////////////////////////////////////////////////////////////////////
  //
  //    Try[T] :
  //
  //    object Try{
  //       def apply[T](expr: => T): Try[T] =
  //          try Success(expr)
  //          catch { case NonFatal(e) => Failure(t) }
  //    }
  //
  //    abstract class Try[T] {
  //      def flatMap[U](f: T => Try[U]): Try[U] = this match {
  //        case Success(x) => try f(x) catch {case NonFatal(e) => Failure(e)}
  //        case fail: Failure => fail
  //      }
  //    }
  /////////////////////////////////////////////////////////////////////////////////
  import scala.util.Try

  def expr = 1

  def timesHundredTry(i: Int): Try[Int] = {
    /* may throw non-fatal exception before returning : */ Try(i * 100)
  }

  def unitTry(i: => Int) = Try { i }

  println {
    s"""Monad laws for Try[T]:
2. Left Unit:      ${unitTry(expr).flatMap(timesHundredTry) == timesHundredTry(expr)}

But Left unit law could fail because: 'unit()' will never throw non-fatal exception:

   Left Unit :      unit(x)    flatMap f  ==?== f(x)
                    Try{ sth } flatMap f  ==?== f(sth)

                    left side will never throw non-fatal exception
                    right side might throw non-fatal exception

unit() it just wrapping expression with Try(), so non-fatal expressions are returned as Failure - not thrown
While, right side of equation may throw non-fatal expressions

Even if Try is not a Monad, it is still good for for-comprehension, cause it has flatMap/unit/map
      """
  }

  /**
   * Kleisli arrows (functions) - are functions which produce Monad without taking any monad as parameter:
   *
   *    f: A => Monad[B]
   *
   * They are params for flatMap, or right sides in for-comprehension.
   * They are composable (eg in for-comprehension, or using flatMap)
   */

  def k1(i: Int): Option[Int] = Some(i)
  def k2(i: Int): Option[Int] = Some(i * 2)
  def k3(i: Int): Option[Int] = Some(i * 3)
  def k4(i: Int): Option[Int] = Some(i * 4)

  def composingKleisli(i: Int): Option[Int] = for {
    r1 <- k1(i)
    r2 <- k2(r1)
    r3 <- k3(r2)
  } yield r3

}
