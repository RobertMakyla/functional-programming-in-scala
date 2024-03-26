package fp

object aj_TypeClass {

  // contract, trait with generic type
  trait MyEquals[A] {
    def ===(a: A, b: A): Boolean
  }

  // type classes
  implicit val myEqualPerson = new MyEquals[Person] {
    override def ===(a: Person, b: Person): Boolean = a.age == b.age && a.name == b.name
  }

  //sugar OPS
  implicit class MyEqualOps[A: MyEquals](a: A)(implicit eq: MyEquals[A]) {
    def ===(other: A) = eq.===(a, other)
  }
  // or
  //
  //  implicit class MyEqualOps[T: MyEqual](that: T) {
  //    def ===(t: T) = implicitly[MyEqual[T]].===(t, that)
  //    def =|=(t: T) = implicitly[MyEqual[T]].=|=(t, that)
  //  }

  val itShouldBeFalse = Person("Rob", 37) === Person("Mark", 19)
  val itShouldBeTrue = Person("Rob", 37) === Person("Rob", 37)


}

case class Person(name: String, age: Int)
