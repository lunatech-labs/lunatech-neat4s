ThisBuild / organization := "com.lunatech"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.13.11"

lazy val `lunatech-neat4s` = (project in file("."))
  .aggregate(neat)

lazy val neat = (project in file("neat"))
  .settings(
    libraryDependencies ++= Dependencies.neatDependencies
  )

lazy val `experiments-neat-xor` = (project in file("experiments/neat-xor"))

