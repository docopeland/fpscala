package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  //generating all types of heaps
  lazy val genHeap: Gen[H] = oneOf(
    const(empty), for{
      a <- arbitrary[A]
      heap <- oneOf(const(empty),genHeap)
    } yield insert(a, heap)
  )

  //generating non-empty heaps
  lazy val genNonEmptyHeap: Gen[H] = for {
    heap <- arbitrary[H]
    a <- arbitrary[A]
  } yield insert(a, heap)

  //generating an empty heap
  lazy val genEmptyHeap: Gen[H] = const(empty)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  //is an empty heap really empty?
  property("empty heap") = forAll (genEmptyHeap) { heap: H =>
    isEmpty(heap)
  }

  //find the min of one element inserted into an empty heap
  property("min of 1") = forAll { a: Int =>
    findMin(insert(a,empty)) == a
  }

  //find the min of two elements inserted into an empty heap
  property("min of two") = forAll { (a: Int, b: Int) =>
    findMin(insert(a,insert(b, empty))) == Math.min(a,b)
  }

  //inserting an element into an empty heap, then deleting the min
  property("insert and delete element") = forAll { a: Int =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  //inserting two elements into an empty heap, then deleting the min
  property("insert 2 and delete min") = forAll { (a: Int,b: Int) =>
    findMin(deleteMin(insert(a,insert(b,empty)))) == Math.max(a,b)
  }

  //finding the min of two non empty heaps should be the min of one or the other
  property("melding") = forAll (genNonEmptyHeap,genNonEmptyHeap) { (heap1: H, heap2: H) =>
    findMin(meld(heap1,heap2)) == Math.min(findMin(heap1),findMin(heap2))
  }

  //checking melding
  property("meld check") = forAll (genNonEmptyHeap,genNonEmptyHeap) { (heap1: H, heap2: H) =>
    isEqual(meld(heap1,heap2),meld(deleteMin(heap1),insert(findMin(heap1),heap2)))
  }

  //function to check heap equality
  def isEqual(heap1: H, heap2: H): Boolean ={
    if (isEmpty(heap1) && isEmpty(heap2)) true
    else {
      (findMin(heap1) == findMin(heap2)) && (isEqual(deleteMin(heap1),deleteMin(heap2)))
    }
  }

  //sorted a sequence of elements when continually finding and deleting minima
  property("sorting") = forAll { (heap: H) =>
    isSorted(heap)
  }
  // function for sorting property
  def isSorted(heap: H): Boolean = {
    if (isEmpty(heap)) true
    else {
      val min = findMin(heap)
      val newH: H = deleteMin(heap)
      isEmpty(newH) || (min <= findMin(newH) && isSorted(newH))
    }
  }
}
