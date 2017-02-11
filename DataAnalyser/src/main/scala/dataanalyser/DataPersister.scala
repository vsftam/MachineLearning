package dataanalyser

import slick.driver.MySQLDriver.api._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}

object DataPersister {

  val logger = org.log4s.getLogger

  val timeout : Int = 10000

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

  def insertStockData(data: List[YahooFinanceDataRow]) : Boolean = {
    // val db = Database.forConfig("mydb")
    val db = getDb
    try {
      logger.info("Driver loaded for mydb")

      if(testDb(db))
        logger.info("Connected to mydb")
      else
        logger.info("Cannot connect to mydb")

      logger.info(s"Finish run testDb")

      true
    } finally db.close
  }
}