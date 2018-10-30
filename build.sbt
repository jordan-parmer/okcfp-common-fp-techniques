name := "okcfp.common-fp-techniques"
version := "1.0.0-SNAPSHOT"
scalaVersion := "2.11.8"

val scalazVersion = "7.2.9"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % scalazVersion
)

scalacOptions += "-feature"

initialCommands in console := "import scalaz._, Scalaz._"
