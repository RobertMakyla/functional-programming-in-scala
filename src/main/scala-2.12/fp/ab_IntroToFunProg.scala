package fp

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future, Promise}
import scala.util.{Failure, Success, Try}

object ab_IntroToFunProg {

  object PolymorphicFunctions {

    /*
     * Polymorphic functions work with any type of data (generics)
     */
    @tailrec
    def isSorted[A](ls: List[A], f: (A, A) => Boolean, acc: Boolean = true): Boolean = ls match {
      case _ if ls.size < 2 => acc
      case h :: second :: _ => isSorted(ls.tail, f, acc && f(h, second))
    }

  }

  object HigherOrderFunctions {

    /*
     * Currying - instead of feeding function with all params in a flat way, we can do it partially
     *            and return other function partially specified
     */
    def curry[A, B, C](f: (A, B) => C): A => (B => C) =
      (a: A) => f(a, _)

    def uncurry[A, B, C](f: A => B => C): (A, B) => C =
      (a: A, b: B) => f(a)(b)

  }

  object FunctionalDataStructures {

    /**
     * Compute the length of a list using foldRight
     *
     * def length[A](as: List[A]): Int
     */
    def length[A](as: List[A]): Int = as.foldRight(0)((elem: A, acc: Int) => acc + 1 )

    //as.foldLeft(0)((acc, _) => acc + 1)

    /**
     * Implement foldLeft that is tail recursive
     *
     * def foldLeft[A,B](ls: List[A], z: B)(f: (B, A) => B): B
     */
    @tailrec
    def foldLeft[A, Z](ls: List[A], z: Z)(f: (Z, A) => Z): Z = ls match {
      case Nil => z
      case h :: tail => foldLeft(tail, f(z, h))(f)
    }

    /**
     * reverse a list using foldLeft
     */
    def reverse[A](ls: List[A]): List[A] = ls.foldLeft(List.empty[A])((acc, elem) => elem :: acc)

    /**
     * implement foldRight using foldLeft
     *
     * list(a, b, c)
     *
     * fold left:                 (z) -> a -> b -> c
     * fold right via fold left:  (z) -> c -> b -> a
     */
    def foldRight[E, Z](ls: List[E], acc: Z)(f: (E, Z) => Z): Z = ls.reverse.foldLeft(acc)((acc: Z, elem: E) => f(elem, acc))

    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    // so to implement foldRight in terms of foldLEft we need to :
    // - reverse the list
    // - and change order of arguments in function f
    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


    /**
     * flatten - using fold (has to be foldRight because foldLeft is reversing a list
     */
    def flatten1[A](ls: List[List[A]]): List[A] = ls.foldRight(List.empty[A])((elem: List[A], acc: List[A]) => elem ++ acc)

    def flatten2[A](ls: List[List[A]]): List[A] = ls.flatten

    /**
     * flatten - using for comprehension
     */
    def flatten3[A](ls: List[List[A]]): List[A] =
      for {
        l <- ls
        elem <- l
      } yield elem

    /**
     * flatten - using flatMap
     */

    //List[A] .map(f: A=>B )            : List [B]
    //List[A] .flatMap(f: A=>List[B] )  : List [B]
    def flatten4[A](ls: List[List[A]]): List[A] = ls.flatMap(identity)


    /**
     * implement map using fold: (foldRight is not reversing the list)
     *
     * def map[A,B](as: List[A])(f: A => B): List[B]
     */
    def map[A, B](as: List[A])(f: A => B): List[B] = as.foldRight(List.empty[B])((elem: A, acc: List[B]) => f(elem) :: acc)

    /**
     * Write a function filter, using foldRight:
     *
     * def filter[A](as: List[A])(f: A => Boolean): List[A]
     */
    def filter[A](as: List[A])(f: A => Boolean): List[A] = as.foldRight(List.empty[A])((elem: A, acc: List[A]) => if (f(elem)) elem :: acc else acc)

    /**
     * implement flatMap using fold:
     *
     * def flatMap[A,B](as: List[A])(f: A => List[B]): List[B]
     */
    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = as.foldRight(List.empty[B])((elem: A, acc: List[B]) => f(elem) ++ acc)

    /**
     * filter, using flatMap:
     */
    def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] = as.flatMap(a => if (f(a)) List(a) else Nil)
    /**
     * zip adding Int's
     */
    @tailrec
    def zip(a: List[Int], b: List[Int], acc: List[Int] = Nil): List[Int] = (a, b) match {
      case (Nil, Nil) => acc.reverse
      case (Nil, bh :: bt) => zip(Nil, bt, bh :: acc)
      case (ah :: at, Nil) => zip(at, Nil, ah :: acc)
      case (ah :: at, bh :: bt) => zip(at, bt, (ah + bh) :: acc)
    }

    // append using fold
    def appendWithFoldRight[A](ls: List[A], elem: A): List[A] = ls.foldRight(List(elem))((elem, acc) => elem :: acc)

    def appendWithFoldLeft[A](ls: List[A], elem: A): List[A] = ls.reverse.foldLeft(List(elem))((acc, elem) => elem :: acc)

    // prepend using fold
    def prependWithFoldLeft[A](ls: List[A], elem: A): List[A] = ls.foldLeft(List(elem))((acc, elem) => elem :: acc).reverse

    def prependWithFoldRight[A](ls: List[A], elem: A): List[A] = ls.reverse.foldRight(List(elem))((elem, acc) => elem :: acc).reverse

    /* implement collect using fold
     *
     * def collect[A, B](ls: List[A], f: PartialFunction[A, B]): List[B]
     */
    def collect[A, B](ls: List[A], f: PartialFunction[A, B]): List[B] =
      ls.foldRight(List.empty[B])((e: A, acc: List[B]) => if (f.isDefinedAt(e)) f.apply(e) :: acc else acc)

  }

  object MyTree {
    /*
     * TREE
     */
    sealed trait Tree[A]


    case class Leaf[A](value: A) extends Tree[A]

    case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

    /** Write a function size that counts the number of nodes (leaves and branches) in a tree */
    def size[A](t: Tree[A]): Int = t match {
      case Branch(l, r) => size(l) + size(r) + 1
      case Leaf(_) => 1
    }

    def height[A](t: Tree[A]): Int = t match {
      case Branch(l, r) => 1 + math.max(height(l), height(r))
      case Leaf(_) => 1
    }

    /** Write a function maximum that returns the maximum element in a Tree[Int] */
    def max(t: Tree[Int]): Int = t match {
      case Branch(l, r) => max(l) max max(r)
      case Leaf(v) => v
    }

    /** Write a function depth that returns the maximum path length from the root of a tree to any leaf */
    def depth[A](t: Tree[A]): Int = t match {
      case Branch(l, r) => 1 + (depth(l) max depth(r))
      case Leaf(_) => 1
    }

    /** Write a function map , analogous to the method of the same name on List ,
     * that modifies each element in a tree with a given function */
    def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
      case Leaf(a) => Leaf(f(a))
    }

    /** Folding a TREE !!!
     *
     * As tree is 2d structure (branches next to each other and branches containing other branches)
     * we need 2 functions to fold it :
     *
     * - f: for mapping value of each leaf
     * - g: for merging left and right piece of branch into one value
     * */
    def fold[A, Z](tree: Tree[A])(f: A => Z)(g: (Z, Z) => Z): Z = tree match {
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
      case Leaf(a) => f(a)
    }

    def sizeViaFold[A](t: Tree[A]): Int =
      fold(t)(_ => 1)(_ + _ + 1) // leftSize + rightSize + 1

    def maxViaFold(t: Tree[Int]): Int =
      fold(t)(identity)(_ max _) // leftMax max rightMax

    def depthViaFold[A](t: Tree[A]): Int =
      fold(t)(_ => 1)((ld, rd) => (ld max rd) + 1) // (leftDepth max rightDepth) + 1

    def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
      fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _)) // Branch(leftMapped, rightMapped)

  }

  object forComprehensionUnderTheHood {

    def maybeString: Option[String] = Some("123")

    def maybeIntFromString(s: String): Option[Int] = Try(s.toInt).toOption

    def maybeStringOnlyNumbers(s: String): Option[String] = if (s.forall(Character.isDigit)) Some(s) else None

    val resultForComrehension = for {
      s <- maybeString // first line = MONAD
      numStr <- maybeStringOnlyNumbers(s) // second = FLATMAP
      i <- maybeIntFromString(numStr) // third = FLATMAP
    } yield i * 100 // third = MAP

    val resultDesugared = maybeString
      .flatMap(s => maybeStringOnlyNumbers(s))
      .flatMap(numStr => maybeIntFromString(numStr))
      .map(i => i * 100)

  }


  object Futures { // source: http://docs.scala-lang.org/overviews/core/futures.html

    def takeYourTime(calculationLabel: String) = for (i <- 1 to 3) {
      print(s"$calculationLabel : $i\n"); Thread.sleep(100)
    }

    case class Name(value: String)

    case class UpperCaseName(value: String)

    case class UpperCaseNameWithSpaces(value: String)

    // long lasting operation
    def makeUpperCase(name: Name): UpperCaseName = {
      takeYourTime(s"Making upper case of $name")
      UpperCaseName(name.value.toUpperCase)
    }

    // long lasting operation
    def addSpaces(upperCaseName: UpperCaseName): UpperCaseNameWithSpaces = {
      takeYourTime(s"Adding spaces to $upperCaseName")
      UpperCaseNameWithSpaces(upperCaseName.value.mkString(" "))
    }

    /**
     * Execution Context
     *
     * It is possible to execute Future's computations in new Thread, Thread pool,
     * or current thread (discouraged cause it's supposed to be in parallel)
     *
     * Futures and Promises always do computations in Execution Context
     *
     * scala.concurrent.ExecutionContext.global - that's global static THREAD POOL
     *
     * the number of threads is number of processors (4):
     * Runtime.getRuntime().availableProcessors()
     */

    val globalEC: ExecutionContextExecutor = scala.concurrent.ExecutionContext.global

    def newAsyncFuture(ec: ExecutionContext) = Future {
      makeUpperCase(Name("async_future"))
    }(ec) //not needed when ec is implicit

    //makeUpperCase - might be already calculating - we can't be sure

    /**
     * Future(...) calls Future.apply() which creates and schedules asynchronous computation in thread pool.
     *
     * This will calculate parallely in separate thread from global thread pool
     * But we don't know when !!!
     *
     * We can stop and wait until it's completed: Await.ready(firstFuture, 10 seconds)
     * We can stop and wait for the result:       Await.result[T](firstFuture, 10 seconds): T
     */

    /**
     * failing Future
     *
     * As it's executed in different thread) encapsulates exceptions just like Try( )
     */
    def newFailingFuture(ec: ExecutionContext): Future[String] = Future(throw new RuntimeException("boom"))(ec)

    /**
     * Callback
     *
     * We have them to process result of the future after it's completed.
     * We don't need to wait for a future to complete with blocking Await.result() / Await.ready().
     *
     * Callback methods are called asynchronously once the future is complete.
     * *
     * onComplete[U](f: Try[T] => U): Unit
     * onSuccess[U](f: PartialFunction[T, U] ): Unit       - Deprecated, use only when we want action after success
     * onFailure[U](f: PartialFunction[Throwable, U]): Unit - Deprecated, use only when we want action after failure
     */

    /**
     * Callback - execution timing
     *
     * Since callback methods need future result, they are executed after future is completed.
     * However not necessarily by the same thread.
     *
     * E.g.: future is completed, then there's no free thread in pool,
     * then EVENTUALLY callback is executed in different free thread.
     */

    /**
     * Callback - execution order
     *
     * They always return: Unit (invocations cannot be chained one after another)
     *
     * We can register many different (or the same) callbacks on a single future.
     * But the order of execution CANNOT be defined:
     *  - it can be executed one after another
     *  - or parallely in different threads from the pool
     *
     * That's why they should never modify anything - there should be no side effects.
     *
     * muFuture onComplete  (...)
     * muFuture onSuccess   ( /* do sth 1*/)
     * muFuture onSuccess   ( /* do sth 2*/)
     * muFuture onComplete  ( /* do yet another thing*/)
     * muFuture onFailure   ( /* log the error */)
     */

    /**
     * Callback - execution conditions
     *
     * Even if some callbacks throw Exception, other are still eventually executed regardless !!!
     * cause it's all in different threads, and always return Unit anyway...
     *
     * Callbacks are executed as long as future is Success/Failure, and type in PartialFunctions matches.
     *
     * eg: onFailure{ t:Throwable     => /* executed at any failure*/ }
     * eg: onFailure{ t:SomeException => /* executed only at SomeException*/ }
     *
     * If some callbacks never complete (e.g. the callback contains an infinite loop),
     * the other callbacks may not get thread to be executed at all.
     *
     * In these cases, a potentially blocking callback must use the blocking{} :
     *
     * Future {
     * blocking {  // - thanks to some default max waiting time, is can avoid deadlock.
     * // some blocking stuff
     * }
     * }
     */

    val aFuture: Future[UpperCaseName] = newAsyncFuture(globalEC)

    aFuture.onComplete {
      case Success(result: UpperCaseName) => println(s"log 1: future finished with $result")
      case Failure(t) => println(s"Error: ${t.getMessage}")
    }(globalEC)

// onSuccess / onFailure - REMOVED IN SCALA 2.13
//    aFuture.onSuccess {
//      case result: UpperCaseName =>
//        println(s"log 2: future finished with $result")
//        throw new RuntimeException("handling success has crashed, but it doesn't affect other callbacks")
//    }(globalEC)
//
//    aFuture.onFailure {
//      case t: Throwable => println(s"Error: ${t.getMessage}")
//    }(globalEC)

    /**
     * Composing Futures - to be executed synchronously (map/flatMap)
     *
     * First it completes makeUpperCase: Name => UpperName
     * Then, it starts addSpaces: UpperName => UpperNameWithSpaces
     *
     * If we gave different execution context to map, then thread pool would be different
     * and so synchronous execution would not be guaranteed !!!!
     */

    def newSyncFuture(ec: ExecutionContext): Future[UpperCaseNameWithSpaces] = Future {
      makeUpperCase(Name("sync_future"))
    }(ec).map {
      upper => addSpaces(upper)
    }(ec)

    /**
     * For-Comprehension (another way of executing synchronously)
     *
     * If any of futures throws an exception, then it stops mapping/flatMapping and returns Failure with first exception ..
     */
    def futureForComprehension(ec: ExecutionContext): Future[String] = {
      implicit val innerEC = ec
      Future("Robert ").flatMap(a => Future(a * 3).flatMap(b => Future(b.toUpperCase).map(c => c)))
    }

    /**
     * foreach() - it's the same as onSuccess()  so just registering asynchronous action in case of success
     *
     * Functions that map/flatMap in case of success (if failure then result is the same):
     *
     * map[U](     pf: PartialFunction[T, U]         ): Future[U]
     * flatMap[U]( pf: PartialFunction[T, Future[U] ]): Future[U]
     *
     * Functions that map/flatMap in case of failure (if success then result is the same):
     *
     * recover[U](     pf: PartialFunction[Throwable, U]         ): Future[U]
     * recoverWith[U]( pf: PartialFunction[Throwable, Future[U] ]): Future[U]
     */

    /**
     * Function that gives another shoot for another future to be successful is:
     *
     * Future[U].fallbackTo(second: Future[U]): Future[U]
     *
     * If first future fails and the second one is successful, then second one will be returned.
     * If both fail, the failure of the first one will be returned.
     */

    /**
     * Function called after future's completed (successfully or not),
     * and still returns this future (so can be chained):
     *
     * Future[U].andThen(pf: PartialFunction[Try[U], Any]): Future[U]
     *
     * eg:  Future { /* ... */}
     * .andThen{ case Success(sth) => log.info(sth)}
     * .andThen{ case Failure(t) => log.error(t)}
     *
     * But we don't really need this chaining it cause we can register many callback methods on future
     * (onComplete: Unit , onSuccess: Unit , onFailure: Unit )
     */

    /**
     * Projection Future[U].failed  - inverting result if we care about future failure
     *
     * If future fails, then it returns Success(Throwable)
     * If future succeeds, then failed returns Failure(NoSuchElementException)
     */

    def futureFailed(ec: ExecutionContext) = newFailingFuture(ec).failed.onComplete {
      case Success(t: Throwable) => println("ok, we got failure as expected")
      case Failure(nse: NoSuchElementException) => println("somwthing went wrong")
    }(ec)

    /**
     * Promises - write-once container which completes a future.
     *
     * Promise.future           - returning the future of this promise (but it's not completing future)
     *
     * Promise.success(t: T): Promise[T]               - COMPLETING future with success !
     * Promise.failure(t: Throwable): Promise[T]       - COMPLETING future with failure !
     * Promise.complete(t: Try[T]): Promise[T]         - COMPLETING future with success or failure, depending on argument
     * Promise.completeWith(f: Future[T]): Promise[T]  - COMPLETING future with success or failure, depending on argument
     *
     * WARNING: if either 2 of these functions are called, it will throw IllegalStateException
     * because we can/should complete Promise only once
     *
     *
     * safer but non-deterministic:
     *
     * Promise.trySuccess(t: T): Boolean          - COMPLETING  if it's not complete yet, returns Boolean if it was complete
     * Promise.tryCailure(t: Throwable): Boolean  - COMPLETING  if it's not complete yet, returns Boolean if it was complete
     * Promise.tryComplete(t: Try[T]): Boolean    - COMPLETING  if it's not complete yet, returns Boolean if it was complete
     */

    def newFutureFromPromise[T](someValue: T)(shouldPass: Boolean): Future[T] = {

      val p = Promise[T]()
      val f = p.future

      println(s"at first, Promise.future is ${if (f.isCompleted) "" else "NOT"} completed ... ")

      if (shouldPass) //but abut the result we can decide long after future is complete.
        p success someValue
      else
        p failure new RuntimeException("you said it should fail")

      println(s"but after using success() or failure(), Promise's future ${if (f.isCompleted) "gets" else "still NOT"} completed ... ")

      f
    }

    /**
     * Sequence of Futures - asynchronous (parallel) execution of many futures
     *
     * Future.sequence(s: Seq[Future[T]]): Future[Seq[T]] completes when either:
     * - all the futures have completed successfully, then in onComplete it matches Success(sequenceOfValues)
     * - one of the futures has failed, then OnComplete it goes to Failure(firstThrowable) and stops waiting for other futures
     */

    def sequenceOfFutures(ec: ExecutionContext) = {
      implicit val ecImplicit = ec

      val manyfutures: Seq[Future[UpperCaseName]] = Seq(newAsyncFuture(ec), newAsyncFuture(ec))

      Future.sequence(manyfutures).onComplete {
        case Success(upperCaseNames: Seq[UpperCaseName]) => println("each future completed successfully")
        case Failure(t: Throwable) => println("some future has failed with " + t.getMessage)
      }

    }
  }

}