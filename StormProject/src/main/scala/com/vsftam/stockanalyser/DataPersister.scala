package com.vsftam.stockanalyser

import java.sql.DriverManager

import scalikejdbc._

/**
  * Created by Vincent on 8/28/16.
  */

class DataPersister {


}

object DataPersister {

  val JDBC_DRIVER = "org.mariadb.jdbc.Driver";
  val DB_URL = "jdbc:mariadb://192.168.1.4:3306/test";
  val USER = "vincenttam"
  val PASSWORD = "vincenttam"

  def init = {
    ConnectionPool.singleton(DataPersister.DB_URL, DataPersister.USER, DataPersister.PASSWORD)
    // ad-hoc session provider on the REPL
    println("--- connection pool initialized")
  }

  def insertStats(stat: String, value: Double) = {
    init
    println(s"--- insertStat for stat $stat with value $value")
    DB autoCommit { implicit session =>
      sql"insert into STATS(stat_type, value) values(${stat}, ${value})".update.apply()
    }
    println(s"--- done insertStat")
  }

  def main(args: Array[String]) : Unit = {
    println("before db config")
    //Class.forName(JDBC_DRIVER)
    println("jdbc driver loaded")
    DataPersister.init
    println("connection established")

    //val stmt = conn.createStatement()
    //val sql = "select * from STATS"
    //stmt.execute(sql)
    //implicit val session = AutoSession    
    
    val entities =  DB readOnly { implicit session =>
      sql"select * from STATS".map(_.toMap).list.apply()
    }

    //val entities = sql"select * from STATS".map(_.toMap).list.apply()

    for(m <- entities;
        k <- m.keys
    ) {
      print(s"Get $k, " + m.get(k) + "\n")
    }

    println("Run sql")
    DataPersister.insertStats("MIN", 200)
  }

}
