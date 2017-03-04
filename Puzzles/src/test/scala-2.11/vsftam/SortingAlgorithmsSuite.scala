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
}
