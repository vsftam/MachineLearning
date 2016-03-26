package vsftam

import breeze.linalg.DenseVector
import breeze.numerics.exp
import vsftam.MathUtils._

/**
  * Created by Vincent on 3/25/16.
  */
object Kernal {

  type KernalFunction = (DenseVector[Double], DenseVector[Double]) => Double

  def linearKernal(x1: DenseVector[Double], x2: DenseVector[Double]): Double = {
    require(x1.length == x2.length)
    x1 dot x2
  }

  def gaussianKernal(x1: DenseVector[Double], x2: DenseVector[Double], sigma: Double): Double = {
    require(x1.length == x2.length)
    exp(-1.0 * sumOfSquares(x1 - x2) / (2 * sigma * sigma))
  }

  def gaussianKernalWithSigma(gk: (DenseVector[Double], DenseVector[Double], Double) => Double, sigma: Double): KernalFunction
  = (x1, x2) => gk(x1, x2, sigma)
}
