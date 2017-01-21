package dataanalyser

import java.nio.file.{Files, Paths}

import dataanalyser.Stat._
import org.log4s._
import org.rogach.scallop._

import scala.io.Source

object RelativeStrengthAnalyser extends App {

  override def main(args: Array[String]) {

    val logger = getLogger

    val conf = new Conf(args)

    val startDate = conf.start()
    val endDate = conf.end()
    val filename = conf.inputfile()
    val returnPeriod = conf.reteriod()
    val relativeStrengthPeriod = conf.rsperiod()

    /* todo:
     * abstract the main process
     */
    var returnsMap = Map[ String, List[Double] ]()
    val tickerList = Source.fromFile(filename).getLines().toList
    // first line is benchmark
    val benchmarkTicker = tickerList.head.split(',')(0)
    var dateIndex = Array[String]()
    
    for( line <- tickerList ) {
      val tokens = line.split(',')   
      val ticker = fixTicker(tokens(1))
      val data = getTickerData(ticker, startDate, endDate, returnPeriod)
      if (dateIndex.length == 0) {
        dateIndex = (data map ( d => d.asOfDate )).toArray
      }
      val rets = nDayReturn( data.map( d => d.adjClose) , returnPeriod ) 
      returnsMap += tokens(0) -> rets
      logger.info(tokens(0) + " (" + tokens(1) + ") :" + rets)
    }

    // logger.info("benchmark is "+ benchmarkTicker)
    // logger.info("map is "+ returnsMap)
    var rsiMap = Map[String, List[Double] ]()
    val benchmarkReturn = returnsMap(benchmarkTicker)
    
    var rsiConsecutivePositiveReturns = Map[String, List[(String,String)]]()
    
    for( ticker <- returnsMap.keys.filter(t => t != benchmarkTicker)) {
      val rs = sequenceDiff(benchmarkReturn, returnsMap(ticker))
      rsiMap += ticker + " vs. " + benchmarkTicker -> rs

      val positivePeriodIndices = getConsecutivePositiveReturnIndices(rs, relativeStrengthPeriod)
      if (positivePeriodIndices.nonEmpty) {
      	rsiConsecutivePositiveReturns += ticker -> ( positivePeriodIndices map ( (i) => ( dateIndex(i._2), dateIndex(i._1) ) ) ) 	    
      }
    }
    
    val seqSizes = rsiMap.values map ( s => s.size )
    
    // header
    logger.info("sequence," + dateIndex.reverse.drop(returnPeriod - 1).mkString(","))
    // fit all seq to the max len, and make it chronological
    for( (seqName, seq) <- rsiMap ) {
       var newSeq = seq.reverse
       while( newSeq.size < seqSizes.max ) {
         newSeq = 0.0 :: newSeq
       }
       val rsString = newSeq map ( d => "%.15f" format d ) mkString ","
      logger.info(seqName + "," + rsString)
    }
    
    // finally print sequence with N consecutive positive returns
    logger.info(rsiConsecutivePositiveReturns.toString)
  }

  def fixTicker(ticker : String) : String = {
    ticker.replace("^", "%5E")
  }
  
  def getTickerData(ticker: String, startDate: Int, endDate: Int, period: Int) : List[YahooFinanceDataRow] = {
      val uri = YahooFinanceDataSource.getURI(ticker, startDate, endDate)
      val dataSource = new YahooFinanceDataSource()
    dataSource.retrieveData(uri)
  }

  class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {

    banner(
      """
        |Usage: RelativeStrengthAnalyser [OPTION]
        |Options:
      """.stripMargin)

    val start = opt[Int](required = true, descr = "Start Date", validate = i => i > 19000101 && i < 99991230)
    val end = opt[Int](required = true, descr = "End Date", validate = i => i > 19000101 && i < 99991230)
    val inputfile = opt[String](required = true, descr = "Input File", validate = f => Files.exists(Paths.get(f)))
    val reteriod = opt[Int](descr = "Return Period", default = Some(20))
    val rsperiod = opt[Int](descr = "Relative Strength Period", default = Some(20))

    validate(start, end) { (s, e) =>
      if (e > s) Right(Unit)
      else Left("end date must be greater than start date")
    }
    verify

    override def onError(e: Throwable): Unit = {
      this.printHelp()
      super.onError(e)
    }
  }
}