package vsftam

import breeze.linalg.{DenseMatrix, DenseVector, norm}

/**
  * Created by Vincent on 3/26/16.
  */
object KmeansClustering {

  def findClosestCentroids(x: DenseMatrix[Double], centroids: DenseMatrix[Double]): DenseVector[Int] = {

    require(x.cols == 2)
    require(centroids.cols == 2)

    val K = centroids.rows
    val indexes = DenseVector.zeros[Int](x.rows)


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
}
