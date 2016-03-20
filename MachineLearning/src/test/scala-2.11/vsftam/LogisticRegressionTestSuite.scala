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

  var data1, data2, x: DenseMatrix[Double] = _
  var theta, y: DenseVector[Double] = _

  before {
    data1 = loadResource("/ex2data1.txt")
    data2 = loadResource("/ex2data2.txt")

    assert(data1.rows === 100)
    assert(data1.cols === 3)
  }

  test("sigmoid function returns 0.5 for 0 vectors") {
    val v = DenseVector(0.0, 0.0, 0.0, 0.0)
    assert(sum(sigmoid(v)) === 0.5 * v.length)

  }

  test("costFunction without regularization should return correct values") {

    val ones: DenseMatrix[Double] = DenseMatrix.ones(data1.rows, 1)
    x = DenseMatrix.horzcat(ones, data1(::, 0 to 1))
    y = data1(::, 2)
    theta = DenseVector.zeros(x.cols)

    val res = computeCost(x, y, theta)
    assert(diffWithinPercentage(res._1, 0.693147, 0.1))
    assert(diffWithinPercentage(res._2(0), -0.100000, 0.1))
    assert(diffWithinPercentage(res._2(1), -12.009217, 0.1))
    assert(diffWithinPercentage(res._2(2), -11.262842, 0.1))
  }

  test("costFunction with regularization should return correct values") {

    x = mapFeature(data2(::, 0), data2(::, 1), 6)
    y = data2(::, 2)
    theta = DenseVector.zeros(x.cols)
    val lambda = 1
    val res = computeCost(x, y, theta, lambda)
    assert(diffWithinPercentage(res._1, 0.693, 0.1))
  }

  test("gradient descent against data set") {
    val alpha = 0.0014
    val iteration = 2000000 // much slower to converge

    // var finalTheta = gradientDescent(x, y, theta, alpha, iteration)
    // assert(finalTheta(0) === -25.161272)
    // assert(finalTheta(1) === 0.206233)
    // assert(finalTheta(2) === 0.201470)
  }
}
