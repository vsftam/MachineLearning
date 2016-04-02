package vsftam

import breeze.linalg.{DenseMatrix, DenseVector, svd}
import org.scalatest.{BeforeAndAfter, FunSuite}
import vsftam.MathUtils._
import vsftam.PrincipalComponentAnalysis._
import vsftam.TestUtils._

/**
  * Created by Vincent on 4/2/16.
  */
class PrincipalComponentAnalysisTestSuite extends FunSuite with BeforeAndAfter {

  var data1: DenseMatrix[Double] = _

  before {
    data1 = loadResource("/ex7data1.txt")
  }

  test("pca should return the correct value") {
    val res = featureNormalize(data1)
    val s: svd.SVD[DenseMatrix[Double], DenseVector[Double]] = pca(res._1)
    assert(diffWithinPercentage(Math.abs(s.U(0, 0)), 0.707107, 0.01))
    assert(diffWithinPercentage(Math.abs(s.U(1, 0)), 0.707107, 0.01))

    val z = projectData(res._1, s.U, 1)
    assert(diffWithinPercentage(z(0, 0), 1.481274, 0.01))

    val x = recoverData(z, s.U, 1)
    assert(diffWithinPercentage(Math.abs(x(0, 0)), 1.047419, 0.01))
    assert(diffWithinPercentage(Math.abs(x(0, 1)), 1.047419, 0.01))
  }
}
