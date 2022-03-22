package fp

object ak_ImplicitEvidence {
  /*
  T =:= U   - types are equal
  T <:< U   - T is subtype of U
  T <%< U   - T is view-convertible to U
  */
  def firstAndLast[E, C](iterable: C)(implicit evidence: C <:< Iterable[E]) = (iterable.head, iterable.last)

}
