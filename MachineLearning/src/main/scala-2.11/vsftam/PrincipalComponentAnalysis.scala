package vsftam

import breeze.linalg.{DenseMatrix, DenseVector, svd}

/**
  * Created by Vincent on 4/2/16.
  */
object PrincipalComponentAnalysis {

  // Returns an M-by-M matrix U (SVD.U), a vector of singular values (SVD.S), and a N-by-N matrix V'
  //
  def pca(x: DenseMatrix[Double]): svd.SVD[DenseMatrix[Double], DenseVector[Double]] = {

    val a = x.t * x :/ x.rows.toDouble
    svd(a)
  }

  /**
    * x is m by n matrix
    * u is n by n matrix
    * returns m by k matrix
    */
  def projectData(x: DenseMatrix[Double], u: DenseMatrix[Double], k: Int): DenseMatrix[Double] = {

    require(x.cols == u.rows)
    require(u.rows == u.cols)
    require(u.cols >= k)

    var z = DenseMatrix.zeros[Double](x.rows, k)
    for (i <- 0 until k) {
      z(::, i) := (x * u(::, i))
    }

    z
  }

  /**
    * z is m by k matrix
    * u is n by n matrix
    * returns m by n matrix
    */
  def recoverData(z: DenseMatrix[Double], u: DenseMatrix[Double], k: Int): DenseMatrix[Double] = {

    require(u.cols >= k)

    var x = DenseMatrix.zeros[Double](z.rows, u.rows)
    for (i <- 0 until u.rows) {
      x(::, i) := z * u(i, 1 to k).t
    }
    x
  }

}
