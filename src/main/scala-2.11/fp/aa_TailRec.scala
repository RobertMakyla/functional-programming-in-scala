package fp

import scala.annotation.tailrec

object aa_TailRec {

  /**
   * Pure functions :
   *
   * - functions with no side effects
   * - functions referentially transparent (for the same input always give the same output)
   */

  /**
   * Side Effects
   *
   * This is a property of pure functions
   *
   * eg:
   * - modifying variables / DB
   * - throwing an exception
   * - printing to console / drawing on the screen
   *
   * Instead of making some side effects:
   * - def buyCoffee(cc: CreditCard): Coffee = { /* charging CreditCard */ }
   *
   * in FP, functions return the product and side effect:
   * - def buyCoffee(cc: CreditCard): (Coffee, Charge) = {/* no side effects */}
   *
   * In FP, we push the side effects as far as possible to outer layers of the app.
   * We should implement apps with a pure core and a thin layer on the outside that handles effects.
   */

  /**
   * Referential Transparency (RT)
   *
   * This is another property of pure functions.
   * For the same input they always give the same output so they can be replaced with result without changing program.
   *
   * 1 + 1 can be replaced with 2. That's why function .+() is can vbe called referentially transparent.
   *
   * eg of not RT:
   * * val sb = new StringBuilder("Hello")
   * * sb.append(", World")   // Hello, World
   * * sb.append(", World")   // Hello, World, World
   */

  /**
   * Why use Pure Functions ?
   *
   * We don't have to care about multiple points of access to a function (eg: by different references)
   * We don't have to care about in which order function will be called (side effects) or in which moment (RT).
   * We don't have to care about state changes that may occur before/after function's call (RT)
   *
   * We only need to focus on function's body, which make life easier.
   *
   * This makes programs more MODULAR (modules are independent and reusable: COMPOSABLE)
   *
   * Pure Functions are modular and composable because they separate the logic of computation, from :
   * - how to obtain the input
   * - what to do with result
   *
   * Pure Functions are like black box.
   */

  /**
   * factorial(n) = 1 * 2 * 3 * ... * n
   */
  @tailrec
  def factorialDirectWay(n: Int, acc: Int = 1): Int = if (n == 0) acc else factorialDirectWay(n - 1, acc * n)

  @tailrec // continuation passing style
  def factorialCPS(n: Int, i: Int = 1, acc: Int = 1): Int = if (n == 0) acc else factorialCPS(n - 1, i + 1, acc * i)

  /**
   * sum(n) = 0 + 1 + 2 + 3 + ... + n
   */
  @tailrec
  def sumDirectWay(n: Int, acc: Int = 0): Int = if (n == 0) acc else sumDirectWay(n - 1, acc + n)

  @tailrec // continuation passing style
  def sumCPS(n: Int, i: Int = 1, acc: Int = 0): Int = if (n == 0) acc else sumCPS(n - 1, i + 1, acc + i)

  /**
   * fib(n) = fib(n-1) + fib(n-2)
   */
  @tailrec // continuation passing style
  def fibCPS(n: Int, b: Int = 0, a: Int = 1): Int = if (n == 0) a else fibCPS(n - 1, a, a + b)

  /**
   * mouse(n) = mouse(n-1) * mouse(n-3) + 1
   */
  @tailrec // continuation passing style
  def mouseCPS(n: Int, c: Int = 1, b: Int = 1, a: Int = 1): Int = if (n <= 2) a else mouseCPS(n - 1, b, a, a * c + 1)


}
