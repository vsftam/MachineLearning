// lazy val root = (project in file(".")).aggregate(Puzzles, StormProject, MachineLearning, GarageSuite, DataAnalyser)
lazy val root = (project in file(".")).aggregate(Puzzles, StormProject, GarageSuite, DataAnalyser)

lazy val commonSettings = Seq(
  version := "1.0",
  scalaVersion := "2.11.8"
)

lazy val Puzzles = (project).settings(commonSettings: _*)

# lazy val StormProject = (project).settings(commonSettings: _*)
lazy val StormProject = project

// lazy val MachineLearning = (project).settings(commonSettings: _*)
lazy val MachineLearning = project

lazy val GarageSuite = (project).settings(commonSettings: _*)

lazy val DataAnalyser = (project).settings(commonSettings: _*)
