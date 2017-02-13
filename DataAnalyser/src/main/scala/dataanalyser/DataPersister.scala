package dataanalyser

import java.sql.{Date, SQLException, Timestamp}
import java.time.{LocalDate, LocalDateTime, ZoneId}

import slick.driver.MySQLDriver.api._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

object DataPersister {

  val logger = org.log4s.getLogger

  case class Stock(asOfDate: Date, ticker: String, open: Double, high: Double, low: Double, close: Double, volume: Int, adjClose: Double, lastUpdateTs: Timestamp)

  class StockData(tag: Tag) extends Table[Stock](tag, "STOCK_DATA_HISTORY") {
    def asOfDate = column[Date]("AS_OF_DATE")
    def ticker = column[String]("TICKER")
    def open = column[Double]("OPEN")
    def high = column[Double]("HIGH")
    def low = column[Double]("LOW")
    def close = column[Double]("CLOSE")
    def volume = column[Int]("VOLUME")
    def adjClose = column[Double]("ADJ_CLOSE")
    def lastUpdatedTs = column[Timestamp]("LAST_UPDATED_TS")
    def pk = primaryKey("pk_1", (asOfDate, ticker))
    def * = (asOfDate, ticker, open, high, low, close, volume, adjClose, lastUpdatedTs) <> (Stock.tupled, Stock.unapply _)
  }

  val stockData = TableQuery[StockData]

  def getDb : Database = Database.forURL(
      "jdbc:mariadb://192.168.1.4:3306/data",
      driver="org.mariadb.jdbc.Driver",
      user="vincenttam",
      password="vincenttam"
    )

  def testDb(db: Database) : Boolean = {
    val testSql : DBIO[Seq[Int]] = sql"select count(1) as c from STOCK_DATA_HISTORY".as[Int]
    val f: Future[Seq[Int]] = db.run(testSql)

    val result: Try[Seq[Int]] = Await.ready(f, 10 seconds).value.get
    result match {
      case Success(t) => true
      case Failure(e) => logger.info(s"Got exception $e"); false
    }
  }

  def insertStockData(ticker: String, data: List[YahooFinanceDataRow]) : Boolean = {
    // val db = Database.forConfig("mydb")
    val db = getDb
    logger.info("Driver loaded for mydb")

    if (testDb(db))  {
      logger.info("Connected to mydb")

      val rawData: Seq[Stock] = data.map(d =>
        Stock(Date.valueOf(d.asOfDate), ticker, d.open, d.high, d.low, d.close, d.volume, d.adjClose, Timestamp.valueOf(LocalDateTime.now))
      )
      val insertActions = DBIO.seq(stockData ++= rawData)

      try {
        logger.info("Running insert")
        val f: Future[Unit] = db.run(insertActions)
        Await.ready(f, 10 seconds)
        logger.info("Done inserting")
        true
      } catch {
        case e: SQLException => logger.info(s"Got SQL exception $e"); false
        case NonFatal(t) => logger.info(s"Got non fatal error $t"); false
      } finally db.close
    }
    else {
      logger.info("Cannot connect to mydb")
      false
    }
  }
}