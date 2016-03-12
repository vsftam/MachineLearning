/**
  * Created by Vincent on 2/1/16.
  */
import breeze.linalg._
import breeze.stats._

object GradientDescent {

  def gradientDescent(x: DenseMatrix[Double], y: DenseVector[Double], theta: DenseVector[Double],
                      alpha: Double, numIterations: Int): DenseVector[Double] =
  {
    val trainingSize = y.length

    require(theta.length == 2)
    require(x.rows == y.length)
    require(x.cols == 2)
    require(alpha > 0.0)

    for( i <- 0 until numIterations) {

      val thetaCurrent = theta
      val z = x * thetaCurrent - y

      theta(0) = thetaCurrent(0) - alpha * sum(z :* x(::, 0)) / trainingSize
      theta(1) = thetaCurrent(1) - alpha * sum(z :* x(::, 1)) / trainingSize

      val cost = computeCost(x, y, theta)
      // println(f"$i%d : cost is $cost%f")
    }

    theta
  }

  /*
   * x is n by 2 matrix, y is a vector of size n, theta is a vector of size 2
   */
  def computeCost(x: DenseMatrix[Double], y: DenseVector[Double], theta: DenseVector[Double]): Double = {

    require(theta.length == 2)
    require(x.rows == y.length)
    require(x.cols == 2)

    val training_size = y.length
    sumOfSquares(x * theta - y) / (2 * training_size)
  }

  def sumOfSquares(x: DenseVector[Double]): Double = {
    sum( x.map( i => Math.pow(i, 2.0)) )
  }

  def featureNormalize(x: DenseMatrix[Double]): (DenseMatrix[Double], DenseVector[Double], DenseVector[Double]) = {
    (x, 0.0, 0.0)

    // need to convert from DenseMatrix for broadcasting operations
    val xMean: DenseVector[Double] = mean(x(::, *)).t(::, 0)
    val xStddev: DenseVector[Double] = stddev(x(::, *)).t(::, 0)

    val xDemean = x(*, ::) - xMean
    val xNorm = xDemean(*, ::) :/ xStddev

    (xNorm, xMean, xStddev)
  }

  def main(args: Array[String]): Unit =
  {

  }
}
