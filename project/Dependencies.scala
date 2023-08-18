import sbt._

object Versions {
  val akka = "2.8.3"
  val archUnit = "0.14.1"
  val enumeratum = "1.7.3"
  val kamon = "2.6.3"
  val logback = "1.2.3"
  val pureconfig = "0.17.4"
  val scalaTest = "3.2.16"
}

object Dependencies {

  private val akka = Seq(
    "com.typesafe.akka" %% "akka-stream",
    "com.typesafe.akka" %% "akka-actor-typed"
  ).map(_ % Versions.akka)

  private val akkaTest = Seq(
    "com.typesafe.akka" %% "akka-actor-testkit-typed",
    "com.typesafe.akka" %% "akka-stream-testkit"
  ).map(_ % Versions.akka % Test)

  private val archUnit = Seq(
    "com.tngtech.archunit" % "archunit"
  ).map(_ % Versions.archUnit % Test)

  private val enumeratum = Seq(
    "com.beachape" %% "enumeratum"
  ).map(_ % Versions.enumeratum)

  private val kamon = Seq(
    "io.kamon" %% "kamon-bundle",
    "io.kamon" %% "kamon-prometheus"
  ).map(_ % Versions.kamon)

  private val logback = Seq(
    "ch.qos.logback" % "logback-classic"
  ).map(_ % Versions.logback)

  private val pureconfig = Seq(
    "com.github.pureconfig" %% "pureconfig"
  ).map(_ % Versions.pureconfig)

  private val scalaTest = Seq(
    "org.scalatest" %% "scalatest"
  ).map(_ % Versions.scalaTest % Test)

  val rootDependencies =
    archUnit ++
    scalaTest

  val neatDependencies =
    akka ++
    akkaTest ++
    enumeratum ++
    kamon ++
    logback ++
    pureconfig ++
    scalaTest
}
