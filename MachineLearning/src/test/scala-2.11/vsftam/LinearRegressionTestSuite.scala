package vsftam

import breeze.linalg.{DenseMatrix, DenseVector}
import org.scalactic.TolerantNumerics
import org.scalatest._
import vsftam.LinearRegression._
import vsftam.MathUtils._
import vsftam.TestUtils._

/**
  * Created by Vincent on 2/20/16.
  */
class LinearRegressionTestSuite extends FunSuite with BeforeAndAfter {

  implicit val doubleEquality = TolerantNumerics.tolerantDoubleEquality(0.001)

  var data1, data2, x1, x2: DenseMatrix[Double] = _
  var y1, y2: DenseVector[Double] = _

  var theta1, theta2: DenseVector[Double] = _

  before {
    data1 = loadResource("/ex1data1.txt")
    assert(data1.rows === 97)
    assert(data1.cols === 2)

    data2 = loadResource("/ex1data2.txt")
    assert(data2.rows === 47)
    assert(data2.cols === 3)

    val ones1: DenseMatrix[Double] = DenseMatrix.ones(data1.rows, 1)
    val dataX1: DenseMatrix[Double] = data1(::, 0).toDenseMatrix.t

    x1 = DenseMatrix.horzcat(ones1, dataX1)
    y1 = data1(::, 1)

    val ones2: DenseMatrix[Double] = DenseMatrix.ones(data2.rows, 1)
    val dataX2: DenseMatrix[Double] = data2(::, 0 to 1)
    val res = featureNormalize(dataX2)
    x2 = DenseMatrix.horzcat(ones2, res._1)
    y2 = data2(::, 2)

    theta1 = DenseVector.zeros(x1.cols)
    theta2 = DenseVector.zeros(x2.cols)
  }

  test("computeCost should return correct value") {
    val fakeX = DenseMatrix.zeros[Double](4,2)
    for( i <- 0 until fakeX.rows )
      for( j <- 0 until fakeX.cols)
        fakeX(i,j) = i + j

    val fakeTheta = DenseVector[Double](1.0, 2.0)

    val fakeY = DenseVector(2.0, 4.0, 6.0, 8.0)

    //val a = x * theta - y
    val a = computeCost(fakeX, fakeY, fakeTheta)

    assert(a == 7.0/4)
  }

  test("computeCost against data set") {
    val cost = computeCost(x1, y1, theta1)
    assert(cost === 32.073)

    val cost2 = computeCost(x2, y2, theta2)
    assert(diffWithinPercentage(cost2, 65591548106.0, 0.01))
  }

  test("gradientDescent against data set") {
    var iteration = 1500
    var alpha = 0.01
    var finalTheta = gradientDescent(x1, y1, theta1, alpha, iteration)
    assert(finalTheta(0) === -3.630291)
    assert(finalTheta(1) === 1.166362)

    iteration = 400
    alpha = 0.03
    finalTheta = gradientDescent(x2, y2, theta2, alpha, iteration)

    assert(finalTheta(0) === 340410.918973)
    assert(finalTheta(1) === 110308.113371)
    assert(finalTheta(2) === -6326.538108)

    val thetaCloseForm = normalEquation(x2, y2)

    assert(diffWithinPercentage(finalTheta(0), thetaCloseForm(0), 1))
    assert(diffWithinPercentage(finalTheta(1), thetaCloseForm(1), 1))
    assert(diffWithinPercentage(finalTheta(2), thetaCloseForm(2), 1))
  }

  test("featureNormalize against data set") {
    val res = featureNormalize(data1)
    assert(res._2(0) === 8.1598)
    assert(res._2(1) === 5.8391)

    assert(res._3(0) === 3.86988)
    assert(res._3(1) === 5.51026)

    val res2 = featureNormalize(data2)
    assert(res2._2(0) === 2000.680851)
    assert(res2._2(1) === 3.170212766)
    assert(res2._2(2) === 340412.6596)

    assert(res2._3(0) === 794.7023535)
    assert(res2._3(1) === 0.760981887)
    assert(res2._3(2) === 125039.8996)
  }


}
