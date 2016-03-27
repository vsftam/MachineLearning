package vsftam

import breeze.linalg.{DenseMatrix, DenseVector, sum}
import org.scalatest.{BeforeAndAfter, FunSuite}
import vsftam.Kernal._
import vsftam.MathUtils._
import vsftam.SupportVectorMachine._
import vsftam.TestUtils._

/**
  * Created by Vincent on 3/25/16.
  */
class KernalTestSuite extends FunSuite with BeforeAndAfter {

  var data: DenseMatrix[Double] = _

  before {
    data = loadResource("/ex6data1.txt")
    assert(data.rows === 51)
    assert(data.cols === 3)
  }

  test("Gaussian Kernal should return correct value") {
    val x1 = DenseVector(1.0, 2.0, 1.0)
    val x2 = DenseVector(0.0, 4.0, -1.0)
    val sigma = 2.0
    val sim = gaussianKernal(x1, x2, sigma)
    assert(diffWithinPercentage(sim, 0.324652, 0.01))
  }

  test("test filter by index") {
    val x = DenseMatrix((1.0, 2.0), (3.0, 4.0), (5.0, 6.0))
    val y = DenseVector(1, 0, 1)
    val f = filterByIndex(x, y)
    assert(f.rows === 2)
    assert(sum(f(0, ::)) === 3.0)
    assert(sum(f(1, ::)) === 11.0)
  }

  test("test SVM with linearKernal") {

    val x = data(::, 0 to 1)
    val y = data(::, 2)

    val cReg = 1
    val model = svmTrain(x, y, cReg, Kernal.linearKernal, 0.001, 20)

    // FIXME: verify test case

    //assert(model._1.rows === 12)  // x
    //assert(model._2.rows === 12)  // y
    //assert(sum(model._2) === 0.0)
    //assert(model._4 === -10.408)  // b
    //assert(model._5.rows === 12)  // alphas
    //assert(model._6.length === 2)  // w
    //assert(model._6(0) === 1.3990) // w
    //assert(model._6(1) === 2.1416)
  }
}
