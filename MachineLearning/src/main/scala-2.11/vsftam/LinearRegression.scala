package vsftam

/**
  * Created by Vincent on 2/1/16.
  */
import breeze.linalg._
import vsftam.MathUtils._

object LinearRegression {

  def gradientDescent(x: DenseMatrix[Double], y: DenseVector[Double], theta: DenseVector[Double],
                      alpha: Double, numIterations: Int): DenseVector[Double] =
  {
    val trainingSize = y.length
    val featureSize = x.cols

    require(theta.length == x.cols)
    require(x.rows == y.length)
    require(alpha > 0.0)

    for( i <- 0 until numIterations) {

      val thetaCurrent = theta
      val z = x * thetaCurrent - y

      for (j <- 0 until featureSize) {
        theta(j) = thetaCurrent(j) - alpha * sum(z :* x(::, j)) / trainingSize
      }

      val cost = computeCost(x, y, theta)
      // println(f"$i%d : cost is $cost%f")
    }

    theta
  }

  /*
   * x is n by m matrix, y is a vector of size n, theta is a vector of size m
   */
  def computeCost(x: DenseMatrix[Double], y: DenseVector[Double], theta: DenseVector[Double]): Double = {

    require(x.rows == y.length)
    require(x.cols == theta.length)

    val training_size = y.length
    sumOfSquares(x * theta - y) / (2 * training_size)
  }


  def main(args: Array[String]): Unit =
  {

  }
}
