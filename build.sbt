ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.11"

lazy val `hyperneat-scala` = (project in file("."))
  .aggregate(neat)

lazy val neat = (project in file("neat"))
  .settings(
    libraryDependencies ++= Dependencies.neatDependencies
  )

