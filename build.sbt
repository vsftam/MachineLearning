// lazy val root = (project in file(".")).aggregate(Puzzles, StormProject, MachineLearning, Coursera, GarageSuite)
lazy val root = (project in file(".")).aggregate(Puzzles, StormProject, Coursera, GarageSuite)

lazy val Puzzles = project

lazy val StormProject = project

lazy val MachineLearning = project

lazy val Coursera = project

lazy val GarageSuite = project
