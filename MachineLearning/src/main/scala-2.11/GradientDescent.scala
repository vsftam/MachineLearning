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
   * x is n by 2 matrix, y is a vector of size n, theta is a vector of size 2
   */
  def computeCost(x: DenseMatrix[Double], y: DenseVector[Double], theta: DenseVector[Double]): Double = {

    require(theta.length == x.cols)
    require(x.rows == y.length)

    val training_size = y.length
    sumOfSquares(x * theta - y) / (2 * training_size)
  }

  def sumOfSquares(x: DenseVector[Double]): Double = {
    sum( x.map( i => Math.pow(i, 2.0)) )
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

  def main(args: Array[String]): Unit =
  {

  }
}
