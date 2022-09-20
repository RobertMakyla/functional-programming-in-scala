package fp

object aj_TypeClass{

  // contract, trait with generic type
  trait FrenchAdder[T] {
    def ajoute(a: T, b: T): T
    def elementNeutre: T
  }

  // type classes
  implicit val intFrenchAdder = new FrenchAdder[Int] {
    override def ajoute(a: Int, b: Int): Int = a + b
    override def elementNeutre: Int = 0
  }

  implicit val stringFrenchAdder = new FrenchAdder[String] {
    override def ajoute(a: String, b: String): String = a + b
    override def elementNeutre: String = ""
  }

  implicit def listFrenchAdder[E] = new FrenchAdder[List[E]] {
    override def ajoute(a: List[E], b: List[E]): List[E] = a ++ b
    override def elementNeutre: List[E] = List.empty[E]
  }

  // Ops API

  //implicit class FrenchAdderOps[A](value: A) {
  //  def ajoute(b: A)(implicit adder: FrenchAdder[A]): A = adder.ajoute(value, b)
  //  def elementNeutre(implicit adder: FrenchAdder[A]): A = adder.elementNeutre
  //}

  implicit class FrenchAdderElegantOps[A : FrenchAdder](value: A) {
    def ajoute(b: A): A = implicitly[FrenchAdder[A]].ajoute(value, b)
    def elementNeutre: A = implicitly[FrenchAdder[A]].elementNeutre
  }

  /**
   * OTHER example
   */
  case class Person(name: String, age: Int)

  trait MyEqual[A] {
    def ===(a: A, b:A): Boolean
    def =|=(a: A, b:A): Boolean
  }

  class PersonEqual extends MyEqual[Person] {
    override def ===(a: Person, b: Person): Boolean = a.name ==b.name && a.age == b.age
    override def =|=(a: Person, b: Person): Boolean = a.name != b.name || a.age != b.age
  }
  implicit val personEq = new PersonEqual

  implicit class MyEqualOps[T: MyEqual](that: T)(implicit eq: MyEqual[T]) {
    def ===(t: T) = eq.===(t, that)
    def =|=(t: T) = eq.=|=(t, that)
  }
  // or
  //
  //  implicit class newMyEqualOps[T: MyEqual](that: T) {
  //    def ===(t: T) = implicitly[MyEqual[T]].===(t, that)
  //    def =|=(t: T) = implicitly[MyEqual[T]].=|=(t, that)
  //  }


  val isTheSame = Person("Rob", 37)  === Person("MArk", 19)


}
