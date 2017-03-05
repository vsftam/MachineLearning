import org.scalatest.FunSuite
import vsftam.SortingAlgorithms._

/**
  * Created by vincenttam on 3/4/17.
  */
class SortingAlgorithmsSuite extends FunSuite{

  val list = List(4, 6, 3, 1, 5, 2)
  val sortedList = List(1, 2, 3, 4, 5, 6)

  test("QuickSort") {
    assert(quickSort(list) === sortedList)
  }

  test("MergeSort") {
    assert(mergeSort(list) === sortedList)
  }

  test("BubbleSort") {
    assert(bubbleSort(list) === sortedList)
  }

  test("BubbleSort2") {
    assert(bubbleSort2(list) === sortedList)
  }

  test("BubbleSort2 - Nil") {
    assert(bubbleSort2(Nil) === Nil)
  }

  test("BubbleSort2 - 1 item") {
    assert(bubbleSort2(List(1)) === List(1))
  }

  test("InsertionSort") {
    assert(insertionSort(list) === sortedList)
  }

  test("SelectionSort") {
    assert(selectionSort(list) === sortedList)
  }

  test("SelectionSort2") {
    assert(selectionSort2(list) === sortedList)
  }
}
