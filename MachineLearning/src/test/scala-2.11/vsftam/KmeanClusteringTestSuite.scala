package vsftam

import breeze.linalg.{DenseMatrix, DenseVector, sum, svd}
import org.scalatest.{BeforeAndAfter, FunSuite}
import vsftam.KmeansClustering._
import vsftam.MathUtils._
import vsftam.TestUtils._

/**
  * Created by Vincent on 3/26/16.
  */
class KmeanClusteringTestSuite extends FunSuite with BeforeAndAfter {

  var data1: DenseMatrix[Double] = _
  var data2: DenseMatrix[Double] = _


  before {
    data1 = loadResource("/ex7data1.txt")
    data2 = loadResource("/ex7data2.txt")
  }

  test("findClosestCentroids, computeCentroids and runKmeans should return correct value") {

    val centroids = DenseMatrix((3.0, 3.0), (6.0, 2.0), (8.0, 5.0))
    val indexes = findClosestCentroids(data2, centroids)

    // first 3 points
    assert(indexes(0) === 1)
    assert(indexes(1) === 3)
    assert(indexes(2) === 2)
    assert(sum(indexes) === 415)

    val c = computeCentroids(data2, indexes, centroids.rows)

    assert(diffWithinPercentage(c(0, 0), 2.4283, 0.01))
    assert(diffWithinPercentage(c(0, 1), 3.1579, 0.01))
    assert(diffWithinPercentage(c(1, 0), 5.8135, 0.01))
    assert(diffWithinPercentage(c(1, 1), 2.6337, 0.01))
    assert(diffWithinPercentage(c(2, 0), 7.1194, 0.01))
    assert(diffWithinPercentage(c(2, 1), 3.6167, 0.01))

    val res = runKmeans(data2, centroids)

    assert(diffWithinPercentage(res._1(0, 0), 1.9540, 0.01))
    assert(diffWithinPercentage(res._1(0, 1), 5.0256, 0.01))
    assert(diffWithinPercentage(res._1(1, 0), 3.0437, 0.01))
    assert(diffWithinPercentage(res._1(1, 1), 1.0154, 0.01))
    assert(diffWithinPercentage(res._1(2, 0), 6.0337, 0.01))
    assert(diffWithinPercentage(res._1(2, 1), 3.0005, 0.01))
    assert(sum(res._2) === 602)
  }

  test("pca should return the correct value") {
    val res = featureNormalize(data1)
    val s: svd.SVD[DenseMatrix[Double], DenseVector[Double]] = pca(res._1)
    assert(diffWithinPercentage(Math.abs(s.U(0, 0)), 0.707107, 0.01))
    assert(diffWithinPercentage(Math.abs(s.U(1, 0)), 0.707107, 0.01))
  }
}
