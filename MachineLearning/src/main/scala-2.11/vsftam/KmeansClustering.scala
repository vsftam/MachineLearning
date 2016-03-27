package vsftam

import breeze.linalg.{*, DenseMatrix, DenseVector, norm}
import breeze.numerics.floor
import breeze.stats.mean
import vsftam.MathUtils._

/**
  * Created by Vincent on 3/26/16.
  */
object KmeansClustering {

  def findClosestCentroids(x: DenseMatrix[Double], centroids: DenseMatrix[Double]): DenseVector[Int] = {

    require(x.cols == 2)
    require(centroids.cols == 2)

    val K = centroids.rows
    val indexes = DenseVector.zeros[Int](x.rows)

    // FIXME use functional approach
    for (i <- 0 until x.rows) {
      var minDist: Double = 0.0

      for (j <- 1 to K) {
        val dist = norm(x(i, ::) - centroids(j - 1, ::), 2)

        if (j == 1) {
          minDist = dist
          indexes(i) = 1
        }
        else {
          if (dist < minDist) {
            minDist = dist
            indexes(i) = j
          }
        }
      }
    }

    indexes
  }

  def computeCentroids(x: DenseMatrix[Double], indexes: DenseVector[Int], k: Int): DenseMatrix[Double] = {
    require(x.rows == indexes.length)
    require(x.cols == 2)

    var centroids = DenseMatrix.zeros[Double](0, x.cols)

    for (i <- 1 to k) {
      val filtered = filterByIndex(x, indexes, i)
      centroids = DenseMatrix.vertcat(centroids, mean(filtered(::, *)))
    }
    centroids
  }

  def runKmeans(x: DenseMatrix[Double], centroids: DenseMatrix[Double], maxIter: Int = 10): (DenseMatrix[Double], DenseVector[Int]) = {

    require(x.cols == 2)
    require(centroids.cols == 2)

    val k = centroids.rows
    var indexes = DenseVector.zeros[Int](x.rows)
    var c = centroids

    for (i <- 1 to maxIter) {
      indexes = findClosestCentroids(x, c)

      c = computeCentroids(x, indexes, k)
    }

    (c, indexes)
  }

  def kmeansInitCentroids(x: DenseMatrix[Double], k: Int): DenseMatrix[Double] = {
    require(x.cols == 2)
    require(x.rows > k)

    var centroids = DenseMatrix.zeros[Double](0, x.cols)
    val rand = scala.util.Random

    for (i <- 1 to k) {
      val index = floor(x.rows * rand.nextFloat()).toInt
      DenseMatrix.vertcat(centroids, x(index, ::).inner.asDenseMatrix)
    }
    centroids
  }
}
