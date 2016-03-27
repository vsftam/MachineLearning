package vsftam

import breeze.linalg.{DenseMatrix, DenseVector, max, min, sum}
import breeze.numerics.{abs, floor}
import vsftam.Kernal.KernalFunction
import vsftam.MathUtils._

/**
  * Created by Vincent on 3/25/16.
  *
  * Train an SVM classifier using simplified version of the SMO algorithm
  */
object SupportVectorMachine {

  def svmTrain(x: DenseMatrix[Double], y: DenseVector[Double], cReg: Double, kf: KernalFunction,
               tol: Double = 0.001, maxPass: Int = 5) = {

    require(x.rows == y.length)

    val m = x.rows
    val n = x.cols

    val y2 = y.map(i => if (i == 0) -1.0 else i)

    val alphas = DenseVector.zeros[Double](m)
    val E = DenseVector.zeros[Double](m)
    var b = 0.0
    var eta = 0.0
    var L = 0.0
    var H = 0.0
    var passes: Int = 0

    // FIXME: us KernalFunction from parameter
    // for Linear Kernal
    val K = x * x.t

    val rand = scala.util.Random

    while (passes <= maxPass) {
      var numChangeAlphas = 0

      for (i <- 0 until m) {

        //E(i) = b + sum (alphas.*Y.*K(:,i)) - Y(i);
        E(i) = b + sum(alphas :* (y2 :* K(::, i))) - y2(i)

        //if ((Y(i)*E(i) < -tol && alphas(i) < C) || (Y(i)*E(i) > tol && alphas(i) > 0)),
        if (y2(i) * E(i) < -tol && alphas(i) < cReg || y2(i) * E(i) > tol && alphas(i) > 0) {

          var j: Int = floor(m * rand.nextFloat).toInt
          while (j == i) {
            j = floor(m * rand.nextFloat).toInt
          }

          E(j) = b + sum(alphas :* y2 :* K(::, j)) - y2(j)

          var iAlphaPrev: Double = alphas(i)
          var jAlphaPrev: Double = alphas(j)

          if (y2(i) == y2(j)) {
            L = max(0.0, alphas(j) + alphas(i) - cReg)
            H = min(cReg, alphas(j) + alphas(i))
          }
          else {
            L = max(0.0, alphas(j) - alphas(i))
            H = min(cReg, cReg + alphas(j) - alphas(i))
          }

          if (L != H) {
            eta = 2 * K(i, j) - K(i, i) - K(j, j)
            if (eta < 0) {

              alphas(j) = alphas(j) - (y2(j) * (E(i) - E(j))) / eta

              alphas(j) = min(H, alphas(j))
              alphas(j) = max(L, alphas(j))

              if (abs(alphas(j) - jAlphaPrev) < tol) {
                alphas(j) = jAlphaPrev
              }
              else {
                alphas(i) = alphas(i) + y2(i) * y2(j) * (jAlphaPrev - alphas(j))

                val b1 = b - E(i) - y2(i) * (alphas(i) - iAlphaPrev) * K(i, j) - y2(j) * (alphas(j) - jAlphaPrev) * K(i, j)
                val b2 = b - E(j) - y2(i) * (alphas(i) - iAlphaPrev) * K(i, j) - y2(j) * (alphas(j) - jAlphaPrev) * K(j, j)

                if (0 < alphas(i) && alphas(i) < cReg) {
                  b = b1
                }
                else if (0 < alphas(j) && alphas(j) < cReg) {
                  b = b2
                }
                else {
                  b = (b1 + b2) / 2
                }

                numChangeAlphas = numChangeAlphas + 1
              }


            }
          }
        }
      }
      if (numChangeAlphas == 0)
        passes = passes + 1
      else
        passes = 0
    }

    val idx = alphas.map(i => if (i > 0.0) 1 else -1)
    val retX = filterByIndex(x, idx)
    val retY = filterByIndex(y2.toDenseMatrix.t, idx)
    val retKf = kf
    val retB = b
    val retAlphas = filterByIndex(alphas.toDenseMatrix.t, idx)
    val retW = ((alphas :* y2).t * x).t

    (retX, retY, retKf, retB, retAlphas, retW)
  }
}
