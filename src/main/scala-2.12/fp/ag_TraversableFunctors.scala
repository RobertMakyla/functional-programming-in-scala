package fp

object ag_TraversableFunctors {

  /**
   * Traversable functor - has 2 functions for transforming data types:
   */

  trait TraversableList[F[_]] {

    def sequence[A](fas: List[F[A]]): F[List[A]]

    def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]]
  }

  object TraversableOptions extends TraversableList[Option] {

    override def sequence[A](fas: List[Option[A]]): Option[List[A]] =
      if (fas.forall(_.isDefined)) Some(fas.map(_.get)) else None

    override def traverse[A, B](as: List[A])(f: (A) => Option[B]): Option[List[B]] = {
      val res: List[Option[B]] = as.map(f)
      sequence(res)
    }
  }


}
