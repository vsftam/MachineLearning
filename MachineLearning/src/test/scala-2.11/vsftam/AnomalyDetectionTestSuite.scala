package vsftam

import breeze.linalg.DenseMatrix
import org.scalatest.{BeforeAndAfter, FunSuite}
import vsftam.AnomalyDetection._
import vsftam.MathUtils._
import vsftam.TestUtils._

/**
  * Created by Vincent on 4/3/16.
  */
class AnomalyDetectionTestSuite extends FunSuite with BeforeAndAfter {

  var data: DenseMatrix[Double] = _
  var dataThreshold: DenseMatrix[Double] = _

  before {
    data = loadResource("/ex8data1.txt")
    dataThreshold = loadResource("/ex8thresholds.txt")
  }

  test("matrixVariance should return correct values") {
    val a = DenseMatrix((1.0, 2.0), (3.0, 4.0), (5.0, 6.0), (7.0, 8.0))
    val matrixVar = matrixVariance(a)
    assert(matrixVar(0) == 5.0)
    assert(matrixVar(1) == 5.0)
  }

  test("multivariateGaussian should return correct values") {
    val x = data(::, 0 to 1)
    val xVal = data(::, 2 to 3)
    val yval = data(::, 4)

    val mu = matrixMean(x)
    val sigma2 = matrixVariance(x)
    val pValCalc = multivariateGaussian(xVal, mu, sigma2)
    val pVal = dataThreshold(::, 1)

    assert(pValCalc.length == pVal.length)
    for (i <- 0 until pVal.length) {
      assert(diffWithinPercentage(pValCalc(i), pVal(i), 0.001))
    }
  }

  test("selectThreshold should return correct values") {
    val yVal = dataThreshold(::, 0)
    val pVal = dataThreshold(::, 1)
    val res = selectThreshold(yVal, pVal)

    assert(diffWithinPercentage(res._1, 0.000089909, 0.001))
    assert(diffWithinPercentage(res._2, 0.875, 0.001))
  }
}
