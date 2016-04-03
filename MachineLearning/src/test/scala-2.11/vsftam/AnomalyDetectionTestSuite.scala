package vsftam

import breeze.linalg.DenseMatrix
import org.scalatest.{BeforeAndAfter, FunSuite}
import vsftam.AnomalyDetection._
import vsftam.TestUtils._

/**
  * Created by Vincent on 4/3/16.
  */
class AnomalyDetectionTestSuite extends FunSuite with BeforeAndAfter {

  var data: DenseMatrix[Double] = _
  before {
    data = loadResource("/ex8thresholds.txt")
  }

  test("selectThreshold should return correct values") {
    val yVal = data(::, 0)
    val pVal = data(::, 1)
    val res = selectThreshold(yVal, pVal)

    assert(diffWithinPercentage(res._1, 0.000089909, 0.001))
    assert(diffWithinPercentage(res._2, 0.875, 0.001))
  }
}
