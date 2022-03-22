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
  implicit class FrenchAdderOps[A](value: A) {
    def ajoute(b: A)(implicit adder: FrenchAdder[A]): A = adder.ajoute(value, b)
    def elementNeutre(implicit adder: FrenchAdder[A]): A = adder.elementNeutre
  }

}
