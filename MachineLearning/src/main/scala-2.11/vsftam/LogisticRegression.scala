package vsftam

/**
  * Created by Vincent on 3/13/16.
  */

import breeze.linalg._
import breeze.numerics.log
import vsftam.MathUtils._

object LogisticRegression {

  /*
   * returns (Cost, Gradient)
   */
  def costFunction(x: DenseMatrix[Double], y: DenseVector[Double], theta: DenseVector[Double]): (Double, DenseVector[Double]) = {
    require(x.rows == y.length)
    require(x.cols == theta.length)

    val trainingSize = y.length

    val s: DenseVector[Double] = sigmoid(x * theta)
    val cost = sum((-y :* log(s)) - ((1.0 - y) :* log(1.0 - s))) / trainingSize

    val g = x(::, *) dot (s - y)
    val grad = g.toDenseVector :/ trainingSize.toDouble

    (cost, grad)
  }
}
