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

sealed abstract class Tree
case class Node(left: Tree, right: Tree, v: Int) extends Tree
case class Leaf(v: Int) extends Tree

object Trees extends Properties("trees") {

  val genLeaf = arbitrary[Int].map(Leaf(_))
  //val genLeaf: Gen[Leaf] = Gen.choose(0, 10).map(Leaf(_))

  val genNode = for {
    v <- arbitrary[Int]
    left <- genTree
    right <- genTree
  } yield Node(left, right, v)

  def genTree: Gen[Tree] = Gen.frequency(
    (100, genNode),
    (110, genLeaf) // leaf picked slightly more often, to prevent stackOverflow
  )

  //def genTree: Gen[Tree] = oneOf(genLeaf, genNode)

  println(genTree.sample)

  property("tree is being generated for testing") =
    forAll(genTree) { t: Tree =>
      println(t)
      // do some testing of tree
      true
    }

}

/**
 * Exercise 8.2
 * What properties specify a function that finds the maximum of a List[Int] ?
 */
object TestingMaxPropertyOfList extends Properties("List.max()") {

  val genInt: Gen[Int] = arbitrary[Int]
  val genListOfInts: Gen[List[Int]] = Gen.nonEmptyListOf(genInt)

  property("max is bigger or equal any element of a list") =
    forAll(genListOfInts) {
      (ls: List[Int]) => ls.forall(_ <= ls.max)
    }

  property("max is in the list") =
    forAll(genListOfInts) {
      (ls: List[Int]) => ls.contains(ls.max)
    }

}