package dataanalyser

import dataanalyser.YahooFinanceDataSource._
import dataanalyser.Stat._

object DataAnalyser extends App {

  println("running main")
   
  override def main(args: Array[String]) {
    
    val usageStr = "Usage: DataAnalyser -start mmmmyydd -end mmmmyydd --ticker ticker"
        
    if (args.length != 6) {
      println("Not sufficent argments ")
      println(usageStr)
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
        case option :: tail => println("Unknown option " + option); println(usageStr); sys.exit(1)
      }
    }
    
    val options = nextOption(Map(), arglist)
    println(options)
    
    if (options.get('start).isEmpty || options.get('end).isEmpty || options.get('ticker).isEmpty) {
      println(usageStr)
      sys.exit(1)
    }

    /* todo:
     * abstract the process to take option map
     * and return list of data
     */
    val dataSource : YahooFinanceDataSource = new YahooFinanceDataSource()
    val startDate = options.get('start).map(_.toInt).getOrElse(throw new IllegalArgumentException)
    val endDate = options.get('end).map(_.toInt).getOrElse(throw new IllegalArgumentException)
    val ticker = options.get('ticker).getOrElse(throw new IllegalArgumentException)
      
    val uri = getURI(ticker, startDate, endDate)
    
    var data : List[YahooFinanceDataRow] = dataSource.retrieveData(uri)

    /* todo:
     * abstract the calculation
     */
    println("total data points: "+ data.size)
    
    val K = 2
    val N = 20

    val bblist : List[(Double, Double)] = getBollingerBands(data map (d => d.adjClose), N, K) 
    bblist map { bb=> println( f"${bb._1}%1.6f" + " : " + f"${bb._2}%1.6f" ) }
 }
  
}
