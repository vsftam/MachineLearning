package vsftam

import breeze.linalg._
import breeze.numerics._
import breeze.stats._

/**
  * Created by Vincent on 3/13/16.
  */
object MathUtils {

  def sumOfSquares(x: DenseVector[Double]): Double = {
    sum(x.map(i => Math.pow(i, 2.0)))
  }

  def featureNormalize(x: DenseMatrix[Double]): (DenseMatrix[Double], DenseVector[Double], DenseVector[Double]) = {

    // need to convert from DenseMatrix for broadcasting operations
    val xMean: DenseVector[Double] = mean(x(::, *)).toDenseVector
    val xStddev: DenseVector[Double] = stddev(x(::, *)).toDenseVector

    val xDemean = x(*, ::) - xMean
    val xNorm = xDemean(*, ::) :/ xStddev

    (xNorm, xMean, xStddev)
  }

  def normalEquation(x: DenseMatrix[Double], y: DenseVector[Double]): DenseVector[Double] = {
    pinv(x.t * x) * x.t * y
  }

  def sigmoid(z: DenseVector[Double]): DenseVector[Double] = {
    pow(exp(-1.0 * z) + 1.0, -1)
  }
}
