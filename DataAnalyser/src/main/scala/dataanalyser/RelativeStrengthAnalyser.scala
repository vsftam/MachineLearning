package dataanalyser

import scala.io.Source
import Stat._

object RelativeStrengthAnalyser extends App {

  override def main(args: Array[String]) {
    val usageStr = "Usage: RelativeStrengthAnalyser -start yyyymmdd -end yyyymmdd -inputFile <location> [-returnPeriod returnPeriod] [-relativeStrengthPeriod rsPeriod]"
      
    if (args.length != 6) {
      println("Not sufficent argments ")
      println(usageStr)
      sys.exit(1)
    }

    /* todo:
     * duplcate code
     */
    val arglist = args.toList
    type OptionMap = Map[String, String]
    
    def nextOption(map: OptionMap, list: List[String]) : OptionMap = {
      list match {
        case Nil => map
        case "-start" :: value :: tail => 
          nextOption(map ++ Map("start" -> value), tail)
        case "-end" :: value :: tail => 
          nextOption(map ++ Map("end"-> value), tail)
        case "-inputFile" :: value :: tail => 
          nextOption(map ++ Map("inputFile" -> value), tail)
        case "-returnPeriod" :: value :: tail =>
          nextOption(map ++ Map("returnPeriod" -> value), tail)
        case "-relativeStrengthPeriod" :: value :: tail => 
          nextOption(map ++ Map("relativeStrengthPeriod" -> value), tail)
        case option :: tail => println("Unknown option "+ option); println(usageStr); exit(1)
      }
    }
    
    val options = nextOption(Map(), arglist)
    println(options)
    
    if( options.get("start").isEmpty || options.get("end").isEmpty || options.get("inputFile").isEmpty ) {
      println("Missing option")
      println(usageStr)
      sys.exit(1)
    }
      
    val startDate = options.get("start").map(_.toInt).getOrElse(throw new IllegalArgumentException)
    val endDate = options.get("end").map(_.toInt).getOrElse(throw new IllegalArgumentException)
    val filename = options.get("inputFile").get
    val returnPeriod = options.get("returnPeriod").map(_.toInt).getOrElse(20)
    val relativeStrengthPeriod = options.get("relativeStrengthPeriod").map(_.toInt).getOrElse(20)

    /* todo:
     * abstract the main process
     */
    var returnsMap = Map[ String, List[Double] ]()
    val tickerList = Source.fromFile(filename).getLines().toList
    // first line is benchmark
    val benchmarkTicker = tickerList(0).split(',')(0)
    var dateIndex = Array[String]()
    
    for( line <- tickerList ) {
      val tokens = line.split(',')   
      val ticker = fixTicker(tokens(1))
      val data = getTickerData(ticker, startDate, endDate, returnPeriod)
      if(dateIndex.size == 0) {
        dateIndex = (data map ( d => d.asOfDate )).toArray
      }
      val rets = nDayReturn( data.map( d => d.adjClose) , returnPeriod ) 
      returnsMap += tokens(0) -> rets
      println( tokens(0) + " (" + tokens(1) + ") :" + rets)
    }   
    
    // println("benchmark is "+ benchmarkTicker)
    // println("map is "+ returnsMap)
    var rsiMap = Map[String, List[Double] ]()
    val benchmarkReturn = returnsMap.get(benchmarkTicker).get
    
    var rsiConsecutivePositiveReturns = Map[String, List[(String,String)]]()
    
    for( ticker <- returnsMap.keys.filter(t => t != benchmarkTicker)) {
      val rs = sequenceDiff( benchmarkReturn, returnsMap.get(ticker).get )
      rsiMap += ticker + " vs. " + benchmarkTicker -> rs

      val positivePeriodIndices = getConsecutivePositiveReturnIndices(rs, relativeStrengthPeriod)
      if( positivePeriodIndices.size > 0) {
      	rsiConsecutivePositiveReturns += ticker -> ( positivePeriodIndices map ( (i) => ( dateIndex(i._2), dateIndex(i._1) ) ) ) 	    
      }
    }
    
    val seqSizes = rsiMap.values map ( s => s.size )
    
    // header
    println( "sequence," + dateIndex.reverse.drop(returnPeriod - 1).mkString(",") )
    // fit all seq to the max len, and make it chronological
    for( (seqName, seq) <- rsiMap ) {
       var newSeq = seq.reverse
       while( newSeq.size < seqSizes.max ) {
         newSeq = 0.0 :: newSeq
       }
       val rsString = newSeq map ( d => "%.15f" format d ) mkString ","
       println(  seqName + "," + rsString )
    }
    
    // finally print sequence with N consecutive positive returns
    println( rsiConsecutivePositiveReturns )
  }
  
  def fixTicker(ticker : String) : String = {
    ticker.replace("^", "%5E")
  }
  
  def getTickerData(ticker: String, startDate: Int, endDate: Int, period: Int) : List[YahooFinanceDataRow] = {
      val uri = YahooFinanceDataSource.getURI(ticker, startDate, endDate)
      val dataSource = new YahooFinanceDataSource()
      dataSource.retrieveData(uri) 
  } 
}