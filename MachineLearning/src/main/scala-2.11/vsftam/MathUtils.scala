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

    val xMean = matrixMean(x)
    val xStddev = matrixStddev(x)

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

  def filterByIndex(x: DenseMatrix[Double], y: DenseVector[Int], k: Int = 1): DenseMatrix[Double] = {
    require(x.rows == y.length)

    var result = DenseMatrix.zeros[Double](0, x.cols)
    for (i <- 0 until x.rows) {
      if (y(i) == k) {
        result = DenseMatrix.vertcat(result, x(i, ::).inner.asDenseMatrix)
      }
    }
    result
  }

  def matrixMean(x: DenseMatrix[Double]): DenseVector[Double] = mean(x(::, *)).toDenseVector

  def matrixStddev(x: DenseMatrix[Double]): DenseVector[Double] = stddev(x(::, *)).toDenseVector

  // var function use 1/(m-1), here we use 1/m
  def matrixVariance(x: DenseMatrix[Double]): DenseVector[Double] = {
    val xMeanSq: DenseMatrix[Double] = pow(x(*, ::) - matrixMean(x), 2)
    (sum(xMeanSq(::, *)) / x.rows.toDouble).toDenseVector
  }

  def multivariateGaussian(x: DenseMatrix[Double], mu: DenseVector[Double], sigma: DenseVector[Double]): DenseVector[Double] = {
    require(x.cols == mu.length)
    require(x.cols == sigma.length)

    val k = mu.length
    val diagSigma = diag(sigma)
    val xDm = x(*, ::) - mu

    val w = (xDm * pinv(diagSigma)) :* xDm
    val res = pow(2 * Math.PI, -k / 2) * pow(det(diagSigma), -0.5) * exp(-0.5 * sum(w(*, ::)))
    res.toDenseVector
  }

  def sumBitVector(b: BitVector): Int = {
    sum(b.map(p => if (p) 1 else 0))
  }
}
