package fp

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck._

object ac_PropertyBasedTesting extends Properties("String") {

  property("startsWith") = forAll { (a: String, b: String) =>
    (a + b).startsWith(a)
  }

  property("endsWith") = forAll { (a: String, b: String) =>
    (a + b).endsWith(b)
  }

  property("substring") = forAll { (a: String, b: String) =>
    (a + b).substring(a.length) == b
  }
}

object ListOfInts extends Properties("reversing list of elems") {

  val genInt: Gen[Int] = Gen.choose(0, 10)
  val genInt2: Gen[Int] = arbitrary[Int]

  val genListOfInts: Gen[List[Int]] = Gen.listOf(genInt)

  property("list keeps order while reversing") =
    forAll(genListOfInts) { ns: List[Int] => ns.reverse.reverse == ns }

}

sealed abstract class Tree{
  def sum: Int
  def maxDepth: Int
}
case class Node(left: Tree, right: Tree, v: Int) extends Tree {
  def sum = left.sum + right.sum + v
  def maxDepth = 1 + (if(left.maxDepth > right.maxDepth) left.maxDepth else right.maxDepth)
}
case class Leaf(v: Int) extends Tree {
  def sum = v
  def maxDepth: Int = 1
}

object Trees extends Properties("trees") {

  val genPositiveInt = Gen.choose(0, 10)

  //val genLeaf = arbitrary[Int].map(Leaf(_))
  val genLeaf: Gen[Leaf] = genPositiveInt.map(Leaf(_))

  val genNode = for {
    v <- genPositiveInt
    left <- genTree
    right <- genTree
  } yield Node(left, right, v)

  def genTree: Gen[Tree] = Gen.frequency(
    (100, genNode),
    (110, genLeaf) // leaf picked slightly more often, to prevent stackOverflow
  )

  //def genTree: Gen[Tree] = oneOf(genLeaf, genNode)

  genTree.sample.foreach { t =>
    println("sample tree          : " + t)
    println("sample tree sum      : " + t.sum)
    println("sample tree maxDepth : " + t.maxDepth)
  }

  property("tree sum") =
    forAll(genTree) { t: Tree => t match {
      case Node(l, r, v) => t.sum == l.sum + r.sum + v
      case Leaf(v) => t.sum == v
      }
    }

}

/**
 * Exercise 8.2
 * What properties specify a function that finds the maximum of a List[Int] ?
 */

object FunctionToTest {
  def myMax(ls: List[Int]): Option[Int] = ls match {
    case Nil => None
    case h :: t =>
      val maxFromTail: Option[Int] = myMax(t)
      maxFromTail.fold(Some(h)) { (someMaxFromTail: Int) =>
        if (h >= someMaxFromTail) Some(h) else Some(someMaxFromTail)
      }
  }
}

object TestingMaxPropertyOfList extends Properties("List.max()") {

  import FunctionToTest._

  val genInt: Gen[Int] = arbitrary[Int]
  val genNonEmptyListOfInts: Gen[List[Int]] = Gen.nonEmptyListOf(genInt)

  property("max is bigger or equal any element of a list") =
    forAll(genNonEmptyListOfInts) {
      (nonEmptyList: List[Int]) => nonEmptyList.forall(_ <= myMax(nonEmptyList).get)
    }

  property("max is in the list") =
    forAll(genNonEmptyListOfInts) {
      (nonEmptyList: List[Int]) => nonEmptyList.contains(myMax(nonEmptyList).get)
    }

}