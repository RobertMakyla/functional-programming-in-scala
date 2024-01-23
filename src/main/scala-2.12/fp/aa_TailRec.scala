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
  def factorial(n: Int, acc: Int = 1): Int = if (n == 1) acc else factorial(n - 1, acc * n)

  /**
   * sum(n) = 0 + 1 + 2 + 3 + ... + n
   *
   * Here I can use direct way because + is associative function: (1 + 2) + 3 == (3 + 2) + 1
   * so iterator n is enough
   *
   * @param n - iterates down, to stop when n == 0
   */
  @tailrec
  def sum(n: Int, acc: Int = 0): Int = if (n == 0) acc else sum(n - 1, acc + n)


  /**
   * fib(n) = fib(n-1) + fib(n-2)
   *
   * n:   1, 2, 3, 4, 5, 6, 7, 8 ...
   * fib: 0, 1, 1, 2, 3, 5, 8, 13 ...
   */
  @tailrec // continuation passing style
  def fib(n:Int, a:Int=0, b:Int=1):Int = {
    if(n==1) a
    else if (n==2) b
    else fib(n-1, b, a+b)
  }

}
