package vsftam

import breeze.linalg.DenseMatrix
import org.scalatest.{BeforeAndAfter, FunSuite}
import vsftam.KmeansClustering._
import vsftam.TestUtils._

/**
  * Created by Vincent on 3/26/16.
  */
class KmeanClusteringTestSuite extends FunSuite with BeforeAndAfter {

  var data: DenseMatrix[Double] = _


  before {
    data = loadResource("/ex7data2.txt")
  }

  test("findClosestCentroids should return correct value") {

    val centroids = DenseMatrix((3.0, 3.0), (6.0, 2.0), (8.0, 5.0))
    val indexes = findClosestCentroids(data, centroids)

    // first 3 points
    assert(indexes(0) == 1)
    assert(indexes(1) == 3)
    assert(indexes(2) == 2)
  }

}
