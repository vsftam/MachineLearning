package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[A]
    h <- oneOf(const(empty), genHeap)
  } yield insert(a, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("findMin") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("insert 2 elements to empty heap") = forAll { (a1: A, a2: A) =>
    var h = insert(a1, empty)
    h = insert(a2, h)
    (a1 min a2) == findMin(h)
  }

  property("insert 2 elements to empty heap and deleteMin") = forAll { (a1: A, a2: A) =>
    var h = insert(a1, empty)
    h = insert(a2, h)
    (a1 max a2) == findMin(deleteMin(h))
  }

  property("insert and deleteMin on empty") = forAll { (a : A) =>
    var h = insert(a, empty)
    h = deleteMin(h)
    isEmpty(h)
  }

  property("order or a heap") = forAll { (h: H) =>
    def checkOrder(h: H, last: A) : Boolean =  {
      if(isEmpty(h)) true
      else {
        val m = findMin(h)
        if( m < last ) false
        else checkOrder(deleteMin(h), m)
      }
    }

    val min = findMin(h)
    checkOrder(deleteMin(h), min)
  }

  property("findMin of meld") = forAll { (h1: H, h2: H) =>
    val m1 = if (isEmpty(h1)) 0 else findMin(h1)
    val m2 = if (isEmpty(h2)) 0 else findMin(h2)
    val h3 = meld(h1, h2)
    val m3 = if (isEmpty(h3)) 0 else findMin(h3)
    (m1 min m2) == m3
  }

  property("deleteMin and insert") = forAll { (h: H) =>
    val m = if(isEmpty(h)) 0 else findMin(h)
    val h2 = deleteMin(h)
    findMin(insert(m, h2)) == m
  }

  property("meld of empty and heap") = forAll { (h: H) =>
    val h2 = meld(h, empty)
    val m = if(isEmpty(h)) 0 else findMin(h)
    val m2 = if(isEmpty(h2)) 0 else findMin(h2)
    m == m2
  }

  property("insert should preserve the order") = forAll { (a: A, h: H) =>
    val m = if(isEmpty(h)) 0 else findMin(h)
    val h2 = insert(a, h)
    (a min m) == findMin(h2)
  }

  property("findMin twice") = forAll { (h: H) =>
    if(isEmpty(h))
      true
    else {
      val m = findMin(h)
      val h2 = deleteMin(h)
      val m2 = if(isEmpty(h2)) m else findMin(h2)
      m2 >= m
    }
  }

  property("insert twice") = forAll { (a1: A, a2: A, h: H) =>
    val m = if(isEmpty(h)) 0 else findMin(h)
    var h2 = insert(a1, h)
    h2 = insert(a2, h2)
    (m min a1 min a2) == findMin(h2)
  }

  property("delete twice") = forAll { (a1: A, a2: A, a3: A) =>
    val h = insert(a1, insert(a2, insert(a3, empty)))
    findMin(deleteMin(deleteMin(h))) == (a1 max a2 max a3)
  }

  property("min value appearing twice") = forAll { (h: H) =>
    val m = if(isEmpty(h)) 0 else findMin(h)
    var h2 = insert(m, h)
    val m1 = findMin(h2)
    h2 = deleteMin(h2)
    m == m1 && findMin(h2) == m1
  }
}
