name := "StockAnalyser"

version := "1.0"

scalaVersion := "2.11.8"

mainClass in (Compile, run) := Some("com.vsftam.stockanalyser.DataPersister")
// mainClass in (Compile, run) := Some("com.vsftam.stockanalyser.LocalTopologyRunner")

libraryDependencies  ++= Seq(
  // other dependencies here
  "org.scalactic" %% "scalactic" % "3.0.1",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "org.apache.storm" % "storm-core" % "1.0.2",
  // ScalikeJDBC
  "org.scalikejdbc" %% "scalikejdbc"       % "2.5.0",
  "com.h2database"  %  "h2"                % "1.4.193",
  "ch.qos.logback"  %  "logback-classic"   % "1.1.7",
  // MariaDB Connect/J
  "org.mariadb.jdbc" % "mariadb-java-client"  % "1.4.6"
)

scalacOptions += "-Yresolve-term-conflict:package"
