package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll
import scala.compiletime.ops.boolean

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  lazy val genHeap: Gen[H] =
    // oneOf(
    // const(empty),
    for {
      n <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(n, h)
  // )

  given Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("gen2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    val minElement = if (a > b) then b else a;
    findMin(h) == minElement
  }

  property("gen3") = forAll { (a: Int) =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  property("gen4") = forAll { (h: H) =>
    val l = getSortedList(h)
    isSorted(l)
  }

  property("gen5") = forAll { (h1: H, h2: H) =>
    if (isEmpty(h1) && isEmpty(h2)) true
    else if (isEmpty(h1) && !isEmpty(h2)) findMin(meld(h1, h2)) == findMin(h2)
    else if (!isEmpty(h1) && isEmpty(h2)) findMin(meld(h1, h2)) == findMin(h1)
    else
      val min1 = findMin(h1)
      val min2 = findMin(h2)
      val min3 = findMin(meld(h1, h2))
      if (min1 <= min2) then min1 == min3 else min2 == min3
  }

  property("gen6") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    val minElement = if (a > b) b else a
    val maxElement = if (a < b) b else a
    val m1 = findMin(h)
    val h2 = deleteMin(h)
    val m2 = findMin(h2)
    m1 == minElement && m2 == maxElement
  }
  property("gen7") = forAll { (a: Int, b: Int, c: Int) =>
    val h = insert(a, insert(b, insert(c, empty)))
    val max = Math.max(a, Math.max(b, c))
    findMin(deleteMin(deleteMin(h))) == max
  }

  def isSorted(xs: List[Int]): Boolean = xs match {
    case Nil      => true
    case x :: Nil => true
    case x :: xs  => (x <= xs.head) && isSorted(xs)
  }

  def getSortedList(h: H): List[Int] =
    if (isEmpty(h))
      Nil
    else
      findMin(h) :: getSortedList(deleteMin(h))
