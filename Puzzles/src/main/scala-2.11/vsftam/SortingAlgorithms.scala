package vsftam

import scala.math.Ordering.Implicits._

/**
  * Created by vincenttam on 3/4/17.
  */
object SortingAlgorithms {

  def bubbleSort[A: Ordering](list: List[A]): List[A] = list match {
    case a :: b :: as => {
      if (a > b) bubbleSort(b :: a :: as)
      else {
        val sorted = bubbleSort(b :: as)
        if (a > sorted.head)
          bubbleSort(sorted.head :: a :: sorted.tail)
        else
          a :: sorted
      }
    }
    case _ => list
  }

  def bubbleSort2[A: Ordering](list: List[A]) : List[A] = {

    def sort[A: Ordering](as: List[A], bs: List[A]) = as match {
      case Nil => bs
      case _ => bubble(as, Nil, bs)
    }

    // bubble the largest value to the end of the list (blist)
    def bubble[A: Ordering](alist: List[A], zlist: List[A], blist: List[A]) : List[A] = alist match {
      case h1 :: h2 :: t => {
        if(h1 > h2) bubble(h1 :: t, h2 :: zlist, blist)
        else bubble(h2 :: t, h1 :: zlist, blist)
      }
      case h1 :: Nil => sort(zlist, h1 :: blist)
    }

    sort(list, Nil)
  }

  def quickSort[A: Ordering](list: List[A]): List[A] = list match {
    case Nil => Nil
    case a :: as => {
      val (left, right) = as.partition(_ < a)
      quickSort(left) ++ (a :: quickSort(right))
    }
  }

  def insertionSort[A: Ordering](list: List[A]): List[A] = {

    def inner[A: Ordering](alist: List[A], blist: List[A]) : List[A] = alist match {
      case Nil => blist
      case a :: as => inner(as, insert(blist, a))
    }

    def insert[A: Ordering](l: List[A], x: A) : List[A] = l match {
      case Nil => List(x)
      case a :: as if x < a => x :: a :: as
      case a :: as => a :: insert(as, x)
    }

    inner(list, Nil)
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
    println("Bubble sort2" + bubbleSort2(list1))
  }
}
