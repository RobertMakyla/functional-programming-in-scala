package fp

object ah_IOMonad {

  /**
   * Implement monad IO[A] which can wrap side effect function in run()
   * The 'A' type is to contain the result type of side effect run(): A
   *
   * It should be a monoid (IO[A], ++, Empty)
   * It should be a monad (unit, flatMap - associativity, left/right identity)
   */
  trait IO[A] {
    self => // so that we can refer to the implementation of this trait using self, instead of this

    def run: A

    def unit(a: A): IO[A] = new IO[A] {def run = a}

    def flatMap[B](f: A => IO[B]): IO[B] = new IO[B] {
      def run = f(self.run).run
    }

    def map[B](f: A => B): IO[B] = new IO[B] {def run = f(self.run)}

    def ++[B](that: IO[B]): IO[B] = new IO[B] {
      def run = {
        self.run
        that.run
      }
    }
  }

  object Empty extends IO[Unit] {
    override def run: Unit = ()
  }

  /** Implement IO monad printing String */
  def printLine(msg: String) = new IO[Unit] {
    def run = println(msg)
  }

  /** Implement IO monad doing some side effect (like readLine from console user) and returning Int */
  def doSideEffectAndReturnOneHundred = new IO[Int] {
    def run = {/* side effect which can block execution, like readLine from user*/ 100}
  }

  /**
   * Since it's a Monad and it has map() - we can play with it (map and forComprehension)
   */

  def twoHundred: IO[Int] = doSideEffectAndReturnOneHundred map (_ * 2)

  def forComprehensionIO: IO[Unit] =
    for {
      _ <- printLine("The number is: ")
      theInt <- twoHundred
      _ <- printLine("" + theInt)
    } yield ()

}
