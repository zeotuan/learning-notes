ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.12"

lazy val root = (project in file("."))
  .settings(
    name := "how-query-engines-work",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.19" % Test,
      "org.apache.arrow" % "arrow-vector" % "18.3.0",
      "org.apache.arrow" % "arrow-memory" % "18.3.0",
      "de.siegmar"       % "fastcsv"      % "4.1.0"
    )
  )
