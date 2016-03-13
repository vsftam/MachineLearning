package vsftam

import java.io.{File, FileNotFoundException}

import breeze.linalg.DenseMatrix
import breeze.numerics._

/**
  * Created by Vincent on 3/13/16.
  */
object TestUtils {

  def diffWithinPercentage(a: Double, b: Double, p: Double): Boolean = {
    abs(a - b) / a < p / 100
  }

  def loadResource(filename: String): DenseMatrix[Double] = {
    val source = getClass.getResource(filename).getFile

    val testFile: File = new File(source)
    if (!testFile.exists()) {
      throw new FileNotFoundException(s"Cannot locate file $testFile")
    }
    breeze.linalg.csvread(testFile)
  }
}
