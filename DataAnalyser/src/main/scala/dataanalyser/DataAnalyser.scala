package dataanalyser

import dataanalyser.Stat._
import dataanalyser.YahooFinanceDataSource._
import org.log4s._

object DataAnalyser extends App {

  override def main(args: Array[String]) {
    
    val usageStr = "Usage: DataAnalyser -start mmmmyydd -end mmmmyydd --ticker ticker"

    val logger = getLogger

    if (args.length != 6) {
      logger.info("Not sufficient arguments ")
      logger.info(usageStr)
      sys.exit(1)
    }
    
    val arglist = args.toList
    type OptionMap = Map[Symbol, String]

    /* todo:
     * function to return Map
     * use mutable map inside
     */
    def nextOption(map: OptionMap, list: List[String]) : OptionMap = {
      list match {
        case Nil => map
        case "-start" :: value :: tail =>
          nextOption( map ++ Map('start -> value), tail)
        case "-end" :: value :: tail =>
          nextOption( map ++ Map('end -> value), tail)
        case "-ticker" :: value :: tail =>
          nextOption( map ++ Map('ticker -> value), tail)
        case option :: tail => logger.info("Unknown option " + option); logger.info(usageStr); sys.exit(1)
      }
    }
    
    val options = nextOption(Map(), arglist)
    logger.info(options.toString)
    
    if (options.get('start).isEmpty || options.get('end).isEmpty || options.get('ticker).isEmpty) {
      logger.info(usageStr)
      sys.exit(1)
    }

    /* todo:
     * abstract the process to take option map
     * and return list of data
     */
    val dataSource : YahooFinanceDataSource = new YahooFinanceDataSource()
    val startDate = options.get('start).map(_.toInt).getOrElse(throw new IllegalArgumentException)
    val endDate = options.get('end).map(_.toInt).getOrElse(throw new IllegalArgumentException)
    val ticker = options.getOrElse('ticker, throw new IllegalArgumentException)
      
    val uri = getURI(ticker, startDate, endDate)

    val data: List[YahooFinanceDataRow] = dataSource.retrieveData(uri)

    /* todo:
     * abstract the calculation
     */
    logger.info("total data points: " + data.size)
    
    val K = 2
    val N = 20

    val bblist: List[(Double, Double)] = getBollingerBands(data map (d => d.adjClose), N, K)
    val msg = bblist map { bb => f"${bb._1}%1.6f" + " : " + f"${bb._2}%1.6f" }
    msg.foreach(m => logger.info(m))
 }
  
}
