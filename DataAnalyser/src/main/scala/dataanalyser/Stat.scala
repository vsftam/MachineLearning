package dataanalyser

import scala.math.sqrt
import scala.collection.mutable.ListBuffer

/* todo:
 * add: type TimeSeries = List[Double]
 */

object Stat {

  def mean(data: List[Double]) : Double = data.sum / data.size
  
  def stddev(data: List[Double]) : Double = {
    val m = mean(data)
    sqrt( data.map(d => (d - m) * (d - m) ).sum / data.size )
  }

  // simple moving average
  def sma(data: List[Double]) : Double = {
    sma(data, data.size)
  }
  
  def sma(data: List[Double], period: Int) : Double = {
    data.take(period).sum / period
  }
  
  def simpleReturn( newData : Double, oldData: Double) : Double = {
    (newData - oldData) / oldData
  }
  
  def nDayReturn(data: List[Double], period: Int) : List[Double] = {
    if(data.size >= period) {
      simpleReturn(data(0), data(period-1)) :: nDayReturn( data.tail, period )
    }
    else {
      Nil
    }
  }
  
  def sequenceDiff(benchmarkReturns: List[Double], stockReturns: List[Double]) : List[Double] = {
    val seqLength = math.min(benchmarkReturns.size, stockReturns.size)
    val diffs = new ListBuffer[Double]() 
    for( i <- 0 until seqLength) {
    	diffs += stockReturns(i) - benchmarkReturns(i) 
    }
    diffs.toList
  }
  
  def getConsecutivePositiveReturnIndices(data: List[Double], requiredPositivePeriods: Int) : List[(Int,Int)] = {
	  // look for k period positive returns
	  var positivePeriods: Int = 0
	  var positivePeriodsIndices = List[(Int,Int)]()
	  var periodStart = 0
	  for( i <- 0 until data.size ) {
	    if( data(i) > 0 ) {
	      if(positivePeriods == 0)
	    	 periodStart = i
	      positivePeriods = positivePeriods + 1
	      if( positivePeriods == requiredPositivePeriods ) {
	        positivePeriodsIndices = (periodStart, i) :: positivePeriodsIndices
	      }
	    }
	    else {
	      positivePeriods = 0;
	    }
	  }
	  positivePeriodsIndices
  }
  
  // Bollinger Bands
  def bollingerBands( data: List[Double], multiplier: Int) : (Double, Double) = {
    val p = data.size
    val ma = sma(data, p)
    val sd = stddev(data)
    
    val r = (ma - multiplier * sd, ma + multiplier * sd)
    // println(" moving avg is "+ma + ", band is " + r)
    r
  }
  
  def getBollingerBands(data : List[Double], period : Int, multiplier: Int) : List[(Double, Double)] = {
	  if( data.size >= period ) {
		  bollingerBands(data take period, multiplier) :: getBollingerBands(data.tail, period, multiplier)
	  }
	  else {
		  Nil
	  }
  }
  
  def percentChange( dt: Double, dtminus1 : Double) : Option[Double] = {
    if (dtminus1 == 0) 
      None
    else
      Some( (dt - dtminus1) / dtminus1 )
  }
}