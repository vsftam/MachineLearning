package vsftam

import breeze.linalg.{DenseVector, max, min}
import vsftam.MathUtils._

/**
  * Created by Vincent on 4/3/16.
  */
object AnomalyDetection {

  def selectThreshold(yVal: DenseVector[Double], pVal: DenseVector[Double]): (Double, Double) = {
    require(yVal.length == pVal.length)

    val stepSize = (max(pVal) - min(pVal)) / 1000.0
    var bestFIscore = 0.0
    var bestEpsilon = 0.0

    for (epsilon <- min(pVal) until max(pVal) by stepSize) {

      val truePredictions = pVal :< epsilon
      val falsePredictions = pVal :>= epsilon
      val truePositive = sumBitVector(truePredictions & (yVal :== 1.0)).toDouble
      val falsePositive = sumBitVector(truePredictions & (yVal :== 0.0)).toDouble
      val falseNegative = sumBitVector(falsePredictions & (yVal :== 1.0)).toDouble

      if (truePositive != 0.0 && truePositive + falsePositive != 0
        && truePositive + falseNegative != 0.0) {
        val precision: Double = truePositive / (truePositive + falsePositive)
        val recall: Double = truePositive / (truePositive + falseNegative)

        val FIscore = 2 * precision * recall / (precision + recall)

        if (FIscore > bestFIscore) {
          bestFIscore = FIscore
          bestEpsilon = epsilon
        }
      }
    }

    (bestEpsilon, bestFIscore)
  }
}
