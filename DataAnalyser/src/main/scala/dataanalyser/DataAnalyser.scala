package dataanalyser

import dataanalyser.Stat._
import dataanalyser.YahooFinanceDataSource._
import org.log4s._
import org.rogach.scallop._

object DataAnalyser extends App {

  override def main(args: Array[String]) {
    val conf = new Conf(args)
    val logger = getLogger

    val dataSource: YahooFinanceDataSource = new YahooFinanceDataSource
    val uri = getURI(conf.ticker(), conf.start(), conf.end())
    val data: List[YahooFinanceDataRow] = dataSource.retrieveData(uri)

    logger.info("total data points: " + data.size)

    DataPersister.insertStockData(data)

    val K = 2
    val N = 20

    val bblist: List[(Double, Double)] = getBollingerBands(data map (d => d.adjClose), N, K)
    val msg = bblist map { bb => f"${bb._1}%1.6f" + " : " + f"${bb._2}%1.6f" }
    msg.foreach(m => logger.info(m))
 }

 
  class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
    banner(
      """
        |Usage: DataAnalyser [OPTION]
        |Options:
      """.stripMargin)
    val start = opt[Int](required = true, descr = "Start Date", validate = i => i > 19000101 && i < 99991230)
    val end = opt[Int](required = true, descr = "End Date", validate = i => i > 19000101 && i < 99991230)
    val ticker = opt[String](required = true, descr = "Stock Ticker")
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
