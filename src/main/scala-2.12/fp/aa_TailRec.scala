package fp

import scala.annotation.tailrec

object aa_TailRec {

  /**
   * factorial(n) = 1 * 2 * 3 * ... * n
   *
   * Here I can use direct way because * is associative function: (1 * 2) * 3 == (3 * 2) * 1
   * so iterator n is enough
   *
   * @param n - iterates down, to stop when n == 0
   */
  @tailrec
  def factorialDirectWay(n: Int, acc: Int = 1): Int = if (n == 0) acc else factorialDirectWay(n - 1, acc * n)

  /**
   * sum(n) = 0 + 1 + 2 + 3 + ... + n
   *
   * Here I can use direct way because + is associative function: (1 + 2) + 3 == (3 + 2) + 1
   * so iterator n is enough
   *
   * @param n - iterates down, to stop when n == 0
   */
  @tailrec
  def sumDirectWay(n: Int, acc: Int = 0): Int = if (n == 0) acc else sumDirectWay(n - 1, acc + n)


  /**
   * fib(n) = fib(n-1) + fib(n-2)
   */
  @tailrec // continuation passing style
  def fibCPS(n: Int, a: Int = 0, b: Int = 1): Int = if (n == 0) b else fibCPS(n - 1, b, a + b)

  /**
   * mouse(n) = mouse(n-1) * mouse(n-3) + 1
   */
  @tailrec // continuation passing style
  def mouseCPS(n: Int, a: Int = 1, b: Int = 1, c: Int = 1): Int = if (n <= 2) c else mouseCPS(n - 1, b, c, c * a + 1)


}
