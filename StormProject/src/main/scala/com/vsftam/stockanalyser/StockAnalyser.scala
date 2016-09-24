package com.vsftam.stockanalyser

import java.io.IOException
import java.nio.charset.Charset
import java.util

import org.apache.storm.{Config, Constants}
import org.apache.storm.shade.org.apache.commons.io.IOUtils
import org.apache.storm.spout.SpoutOutputCollector
import org.apache.storm.task.TopologyContext
import org.apache.storm.topology.{BasicOutputCollector, OutputFieldsDeclarer}
import org.apache.storm.topology.base.{BaseBasicBolt, BaseRichSpout}
import org.apache.storm.tuple.{Fields, Tuple, Values}



/**
  * Created by Vincent on 8/17/16.
  */
object StockAnalyser {


}

class FeedListener extends BaseRichSpout {

  var outputCollector: SpoutOutputCollector = _
  var feed: java.util.List[String] = _
  var next: Integer = 0

  override def declareOutputFields(declarer: OutputFieldsDeclarer): Unit = {
    declarer.declare(new Fields("rawData"))
  }

  override def open(conf: util.Map[_, _], context: TopologyContext, collector: SpoutOutputCollector): Unit = {
    this.outputCollector = collector
    try {
      feed = IOUtils.readLines(ClassLoader.getSystemResourceAsStream("SP500.txt"),
        Charset.defaultCharset().name())
      println("--- Getting feed with size " + feed.size())
    } catch {
      case e: IOException =>
        throw new RuntimeException(e)
    }
  }

  override def nextTuple(): Unit = {
    val row = feed.get(next)
    // println("--- Getting row " + next + "[" + row + "]")
    outputCollector.emit(new Values(row))
    next = (next + 1) % feed.size
  }
}

class StockExtractor extends BaseBasicBolt {

  override def declareOutputFields(declarer: OutputFieldsDeclarer): Unit = {
    declarer.declare(new Fields("Date","Open","High","Low","Close","Volume","AdjClose"))
  }

  override def execute(input: Tuple, collector: BasicOutputCollector): Unit = {
    val row = input.getStringByField("rawData")
    val fields: Array[String] = row.split(",")
    println("--- Splitting for adjClose: "+ fields(6))
    collector.emit( new Values(fields(0), fields(1), fields(2), fields(3), fields(4), fields(5), fields(6)) )
  }

}

class StockAnalyser extends BaseBasicBolt {

  var stats: Map[String, Double] = _

  /*
  override def getComponentConfiguration: util.Map[String, AnyRef] = {
    val conf = new Config
    // need object as config value takes AnyRef, NOT Any
    conf.put(Config.TOPOLOGY_TICK_TUPLE_FREQ_SECS, new Integer(2))
    conf
  }
  */

  override def declareOutputFields(declarer: OutputFieldsDeclarer): Unit = ()

  override def prepare(stormConf: util.Map[_, _], context: TopologyContext): Unit = {
    stats = Map[String, Double]()
  }

  override def execute(input: Tuple, collector: BasicOutputCollector): Unit = {
    println("--- in final bolt")
    val adjClose: Double = input.getStringByField("AdjClose").toDouble
    //println(s"--- Getting adj Close: $adjClose")
    stats.get("MAX") match {
      case Some(d) => if(adjClose > d) {
          //println(s"--- overwriting MAX $d with $adjClose")
          stats = stats + ("MAX" -> adjClose)
        }
      case None => stats = stats + ("MAX" -> adjClose)
    }
    stats.get("MIN") match {
      case Some(d) => if(adjClose < d) {
        //println(s"--- overwriting MIN $d with $adjClose")
        stats = stats + ("MIN" -> adjClose)
      }
      case None => stats = stats + ("MIN" -> adjClose)
    }

    //if(isTickTuple(input))
      printStat
    //else
      //println("--- Not Tick Tuple, not printing")

  }

  def printStat {
    val max = stats.get("MAX").get
    val min = stats.get("MIN").get
    println(s"--- Max is $max, min is $min")
    if( min < max ) {
      DataPersister.insertStats("MAX", max)
      DataPersister.insertStats("MIN", min)
    }
    println(s"--- Done print stats")
  }

  def isTickTuple(tuple:Tuple): Boolean = {
    val sourceComponent = tuple.getSourceComponent
    val sourceStreamId = tuple.getSourceStreamId
    Constants.SYSTEM_COMPONENT_ID.equals(sourceComponent) &&
    Constants.SYSTEM_TICK_STREAM_ID.equals(sourceStreamId)
  }
}