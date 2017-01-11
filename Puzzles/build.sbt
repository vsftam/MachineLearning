name := "Puzzles"

version := "1.0"

scalaVersion := "2.11.8"

mainClass in (Compile, run) := Some("vsftam.NQueens")

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.0.1",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)

resolvers ++= Seq(
  // other resolvers here
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)
