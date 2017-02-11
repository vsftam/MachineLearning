name := "DataAnalyser"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.slf4j" % "slf4j-api" % "1.7.22",
  "org.slf4j" % "slf4j-simple" % "1.7.22",
  "org.log4s" %% "log4s" % "1.3.4",
  "org.rogach" %% "scallop" % "2.0.6",
  "com.typesafe.slick" %% "slick" % "3.1.1",
  // MariaDB Connect/J
  "org.mariadb.jdbc" % "mariadb-java-client"  % "1.4.6"
)
