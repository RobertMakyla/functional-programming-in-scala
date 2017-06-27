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

  def genTree: Gen[Tree] = Gen.frequency( // picked 100/100
    (100, genNode),
    (100, genLeaf)
  )

  //  def genTree: Gen[Tree] = oneOf(genLeaf, genNode)

  println(genTree.sample)

  property("tree is being generated for testing") =
    forAll(genTree) { t: Tree =>
      println(t)
      // do some testing of tree
      true
    }

}