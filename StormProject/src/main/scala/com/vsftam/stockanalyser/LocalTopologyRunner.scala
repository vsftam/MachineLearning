package com.vsftam.stockanalyser

import org.apache.storm.{Config, LocalCluster}
import org.apache.storm.topology.TopologyBuilder
import org.apache.storm.tuple.Fields
import org.apache.storm.utils.Utils

/**
  * Created by Vincent on 8/20/16.
  */
object LocalTopologyRunner {

  def main(args: Array[String]): Unit = {

    val builder = new TopologyBuilder
    builder.setSpout("feed-listener", new FeedListener)
    builder.setBolt("stock-extractor", new StockExtractor)
        .shuffleGrouping("feed-listener")
    builder.setBolt("stock-analyser", new StockAnalyser)
        .fieldsGrouping("stock-extractor", new Fields("Date"))

    val config = new Config
    config.setDebug(true)

    val topology = builder.createTopology

    val cluster = new LocalCluster
    cluster.submitTopology("my-stock-analyser-topology", config, topology)
    // need to add the following line in sbt for Utils.sleep to work
    // scalacOptions += "-Yresolve-term-conflict:package"
    Utils.sleep(30000)
    println("--- killing topology")
    cluster.killTopology("my-stock-analyser-topology")
    println("--- shutting down cluster")
    cluster.shutdown()
  }
}
