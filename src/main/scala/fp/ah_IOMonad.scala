package fp

object ah_IOMonad {

  /**
   * Implement monad IO[A] which can wrap side effect function in run()
   * The 'A' type is to contain the result type of side effect run(): A
   *
   * It should be a monoid (IO[A], ++, Empty)
   * It should be a monad (unit, flatMap - associativity, left/right identity)
   */

  /*
  * hint: trait IO[A]{ def run: A }
  * */
  trait IO[A] {
    self => // so that we can refer to the implementation of this trait using self, instead of this

    def run: A

    def flatMap[B](f: A => IO[B]): IO[B] = IO{ f(self.run).run }

    def unit[B](b: B): IO[B] = IO(b)

    def map[B](f: A => B): IO[B] = flatMap(x => unit(f(x)))

    def ++[B](that: IO[B]): IO[B] = new IO[B] {
      def run: B = {
        this.run
        that.run
      }
    }
  }

  object IO {
    def apply[A](a: => A): IO[A] = new IO[A] {
      override def run: A = a
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
  def doSideEffectAndReturnOneHundred: IO[Int] = IO {
    {/* side effect which can block execution, like readLine from user*/ 100}
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

  /**
   * IO[A] Monad is just a monad returning some value of type A, after running side effects.
   *
   * Benefits:
   * - We can use it to clearly separate pure functional code from side effects.
   * - Since we can use forComprehension, we can craft one IO from many IOs, or play with map/flatMap
   *
   * Problems:
   * - it can hang forever (like readLine when user never enters the value)
   * - we cannot tell it it's done in sync or async way
   *   (our example is doing sync way but we could du run() in separate thread.. like Future)
   * - writing efficient, streaming I/O will generally involve monolithic loops (ex processing file 1 line at time)
   *   (if we don't want to load whole file to memory - streaming is the answer, but processing loops are ugly - we cannot compose them)
   */

  /** Local effects and mutable state
   *
   * Function is pure if it makes no side effects and if it's referentially transparent
   * (same args => always the same result)
   *
   * But it can be still referentially transparent with local effects, and mutable state (only in local scope)
   *
   * eg. come loops with var and changing it's value each iteration - it happens under Scala known functions
   */

  /**
   * Purity is contextual
   *
   * ------------- eg 1: (memory management)
   *
   * Every construction of data (instantiating) has a side effect in Scala.
   *
   *   case class Foo(msg: String)
   *   val x = Foo("hello") // creating unique object in memory and returning reference to this new object.
   *
   * But most Scala programs don't use references, just values (using ==, not eq/equals to compare them)
   * So in such context it's pure..
   *
   * We could track where memory is allocated / what references are created..
   * but since we have automatic memory management we don't want that - it's not interesting for us.
   * IT'S NOT THE CONTEXT IN WHICH WE WANT TO BE PURE !
   *
   * ------------- eg 2: (logging)
   *
   * The log.info("") / println("") can be called as side effect but we need to answer ourselves, what kind of side effect matters?
   * What kind of side effects can influence the way the application works ?
   * logging shouldn't so in our context it's also pure..
   *
   * Most of functions have hidden dependencies on the IO subsystem (logger/System.out.println)
   * or by creating objects in memory and GC'ing them.
   *
   * Making these side effects (like logging, or creating objects) is a choice we make as programmers !
   */
}
