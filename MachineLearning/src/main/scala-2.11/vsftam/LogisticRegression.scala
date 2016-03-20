package vsftam

/**
  * Created by Vincent on 3/13/16.
  */

import breeze.linalg._
import breeze.numerics.log
import vsftam.MathUtils._

object LogisticRegression {

  def gradientDescent(x: DenseMatrix[Double], y: DenseVector[Double], theta: DenseVector[Double],
                      alpha: Double, numIterations: Int): DenseVector[Double] = {
    val trainingSize = y.length
    val featureSize = x.cols

    require(theta.length == x.cols)
    require(x.rows == y.length)
    require(alpha > 0.0)

    for (i <- 0 until numIterations) {

      val thetaCurrent = theta
      val z = sigmoid(x * thetaCurrent) - y

      for (j <- 0 until featureSize) {
        theta(j) = thetaCurrent(j) - alpha * sum(z :* x(::, j)) / trainingSize
      }

      val ret = computeCost(x, y, theta)
      // println(f"$i%d : cost is $cost%f")
    }

    theta
  }

  /*
   * returns (Cost, Gradient)
   */
  def computeCost(x: DenseMatrix[Double], y: DenseVector[Double], theta: DenseVector[Double], lambda: Double = 0.0):
  (Double, DenseVector[Double]) = {
    require(x.rows == y.length)
    require(x.cols == theta.length)

    val trainingSize = y.length

    val s: DenseVector[Double] = sigmoid(x * theta)
    var cost = sum((-y :* log(s)) - ((1.0 - y) :* log(1.0 - s))) / trainingSize

    val g = x(::, *) dot (s - y)
    var grad = g.toDenseVector :/ trainingSize.toDouble

    if (lambda != 0.0) {
      cost = cost + sumOfSquares(theta(1 until theta.length)) * (lambda / (2 * trainingSize))
      grad(1 until grad.length) + theta(1 until theta.length) * (lambda / (2 * trainingSize))
    }

    (cost, grad)
  }
}
