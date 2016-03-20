package vsftam

import breeze.linalg._
import breeze.numerics._
import breeze.stats._

/**
  * Created by Vincent on 3/13/16.
  */
object MathUtils {

  def sumOfSquares(x: DenseVector[Double]): Double = {
    sum(pow(x, 2))
  }

  /**
    *
    * @param x matrix to be normalized
    * @return tuple of (normalized matrix, mean as column vector, standard deviration as column vector)
    */
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

  def sigmoidGradient(z: DenseVector[Double]): DenseVector[Double] = {
    sigmoid(z) :* (1.0 - sigmoid(z))
  }

  def mapFeature(x1: DenseVector[Double], x2: DenseVector[Double], degree: Int): DenseMatrix[Double] = {
    val cols: Int = (degree + 3) * degree / 2 + 1
    val out = DenseMatrix.zeros[Double](x1.length, cols)

    out(::, 0) := 1.0
    var c = 1
    for (
      i <- 1 to degree;
      j <- 0 to i
    ) {
      out(::, c) := pow(x1, i - j) :* pow(x2, j)
      c = c + 1
    }
    out
  }

}
