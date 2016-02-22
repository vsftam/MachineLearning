import java.io.{FileNotFoundException, File}

import breeze.linalg.{Matrix, DenseVector, DenseMatrix}
import org.scalactic.{TolerantNumerics, Tolerance}
import org.scalatest._

import GradientDescent._

/**
  * Created by Vincent on 2/20/16.
  */
class GradientDescentTestSuite extends FunSuite with BeforeAndAfter {

  implicit val doubleEquality = TolerantNumerics.tolerantDoubleEquality(0.001)

  var x: DenseMatrix[Double] = _
  var y: DenseVector[Double] = _
  var theta: DenseVector[Double] = _

  before {
    val data: DenseMatrix[Double] = loadResource("/ex1data1.txt")
    assert(data.rows == 97)
    assert(data.cols == 2)

    val ones: DenseMatrix[Double] = DenseMatrix.fill(data.rows, 1){1.0}
    val dataX: DenseMatrix[Double] = data(::,0).toDenseMatrix.t

    x = DenseMatrix.horzcat( ones, dataX )
    y = data(::,1)
    theta = DenseVector[Double](0, 0)
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
    val cost = computeCost(x, y, theta)
    assert(cost === 32.073)
  }

  test("gradientDescent against data set") {
    val iteration = 1500
    val alpha = 0.01
    val finalTheta = gradientDescent(x, y, theta, alpha, iteration)
    assert(finalTheta(0) === -3.630291)
    assert(finalTheta(1) === 1.166362)
  }

  def loadResource(filename: String): DenseMatrix[Double] = {
    val source = getClass.getResource(filename).getFile

    val testFile: File = new File(source)
    if( !testFile.exists() ) {
      throw new FileNotFoundException(s"Cannot locate file $testFile")
    }
    return breeze.linalg.csvread(testFile)
  }
}
