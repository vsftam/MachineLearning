package dataanalyser

import java.net.URI
import java.time.LocalDate
import java.time.format.DateTimeFormatter


case class YahooFinanceDataRow(
  	asOfDateStr : String,
  	open: Double,
  	high: Double,
  	low: Double,
  	close: Double,
  	volume: Int,
  	adjClose: Double) {
  private val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
  def asOfDate = LocalDate.parse(asOfDateStr, formatter)
}

class YahooFinanceDataSource extends InternetDataSource[YahooFinanceDataRow] {
  
  override def parseline(line: String) : Option[YahooFinanceDataRow] = {
    val tokens = line.split(',')
    
    if(tokens.length != 7) {
      None
    }
    else {
      val dataRow = new YahooFinanceDataRow(
          tokens(0), tokens(1).toFloat, tokens(2).toFloat, tokens(3).toFloat, 
          tokens(4).toFloat, tokens(5).toInt, tokens(6).toFloat
          )
      Some(dataRow)
    }
  }
}

object YahooFinanceDataSource {
  val URI_PREFIX = "http://ichart.finance.yahoo.com/table.csv?s="
  val URI_SUFFIX = "&g=d&ignore=.csv"
    
  def getURI(ticker: String, start: Integer, end: Integer) : URI = {
    val startURI = getDateURI(start, true)
    val endURI = getDateURI(end, false)
    val uri = new URI(URI_PREFIX + ticker + startURI + endURI + URI_SUFFIX)
    println("Getting uri :" + uri)
    uri
  }  
    
  private def getDateURI(date: Int, isStart: Boolean): String = {
    // date in form of yyyymmdd
    val year: Int = date / 10000;
    val month: Int = (date % 10000) / 100 - 1;
    val day: Int = date % 10000 % 100;
    if( isStart ) {
    	"&a="+month+"&b="+day+"&c="+year
    }
    else {
      "&d="+month+"&e="+day+"&f="+year
    }
  } 
}

  