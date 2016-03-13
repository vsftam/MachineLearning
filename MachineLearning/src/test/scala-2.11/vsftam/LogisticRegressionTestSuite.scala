package vsftam

import breeze.linalg.{DenseMatrix, DenseVector, sum}
import org.scalatest.{BeforeAndAfter, FunSuite}
import vsftam.LogisticRegression._
import vsftam.MathUtils._
import vsftam.TestUtils._

/**
  * Created by Vincent on 3/13/16.
  */
class LogisticRegressionTestSuite extends FunSuite with BeforeAndAfter {

  var data, x: DenseMatrix[Double] = _
  var theta, y: DenseVector[Double] = _

  before {
    data = loadResource("/ex2data1.txt")

    assert(data.rows === 100)
    assert(data.cols === 3)

    val ones: DenseMatrix[Double] = DenseMatrix.ones(data.rows, 1)
    x = DenseMatrix.horzcat(ones, data(::, 0 to 1))
    y = data(::, 2)

    theta = DenseVector.zeros(x.cols)
  }

  test("sigmoid function returns 0.5 for 0 vectors") {
    val v = DenseVector(0.0, 0.0, 0.0, 0.0)
    assert(sum(sigmoid(v)) === 0.5 * 4)

  }

  test("costFunction should return correct values") {
    val res = costFunction(x, y, theta)
    assert(diffWithinPercentage(res._1, 0.693147, 0.1))
    assert(diffWithinPercentage(res._2(0), -0.100000, 0.1))
    assert(diffWithinPercentage(res._2(1), -12.009217, 0.1))
    assert(diffWithinPercentage(res._2(2), -11.262842, 0.1))
  }
}
