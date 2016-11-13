package fp

import scala.annotation.tailrec

object aa_TailRec {

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
