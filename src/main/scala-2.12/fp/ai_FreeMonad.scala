package fp

object ai_FreeMonad {

  /**
   * Free Monad - brings together Monads and Interpreters
   *
   *
   * ------------- Monads -------------
   *
   * Monad[A].flatMap(f: A => Monad[B]): Monad[B]  is all about flow control.
   * Composable monads are executed from left to right as long as they should.
   *
   *
   * ----------- Interpreters ---------
   *
   * Interpreters have 1 job: to separate AST (abstract syntax tree) (eg: Multiply(2, Add(3,4)))
   * from the way it is run under the hood (eg: left to right or right to left, or using Double or Int)
   *
   *
   * ----------- Free Monad ------------
   *
   * 1. Provide AST (abstract syntax tree) to express monadic operations
   * 2. Provide API to write interpreters that give meaning to AST
   *
   *
   */

  // nice example it in ZIOPlayground project

}
