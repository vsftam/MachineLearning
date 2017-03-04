package vsftam

import scala.math.Ordering.Implicits._

/**
  * Created by vincenttam on 3/4/17.
  */
object SortingAlgorithms {

  def quickSort[A: Ordering](list: List[A]): List[A] = list match {
    case Nil => Nil
    case a :: as => {
      val (left, right) = as.partition(_ < a)
      quickSort(left) ++ (a :: quickSort(right))
    }
  }


  def mergeSort[A: Ordering](list: List[A]): List[A] = list match {
    case Nil => Nil
    case _ :: Nil => list
    case _ => {
      val (l, r) = list.splitAt(list.length / 2)
      merge(mergeSort(l), mergeSort(r))
    }
  }

  private def merge[A: Ordering](l: List[A], r: List[A]) : List[A] = {

    if(l.length == 0) r
    else if(r.length == 0) l
    else {
      val l1 = l.head
      val r1 = r.head
      if(l1 < r1) l1 :: merge(l.tail, r) else r1 :: merge(l, r.tail)
    }
  }

  def main(args: Array[String]): Unit = {

    val list1 = List(4, 3, 5, 2, 3, 1)

    println("Quick sort: " + quickSort(list1))
    println("Merge sort: " + mergeSort(list1))
  }
}
