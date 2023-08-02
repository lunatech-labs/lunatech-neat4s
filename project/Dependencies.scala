import sbt._

object Versions {
  val akka = "2.8.3"
  val enumeratum = "1.7.3"
  val logback = "1.2.3"
  val scalaTest = "3.2.16"
}

object Dependencies {

  private val akka = Seq(
    "com.typesafe.akka" %% "akka-stream",
    "com.typesafe.akka" %% "akka-actor-typed"
  ).map(_ % Versions.akka)

  private val enumeratum = Seq(
    "com.beachape" %% "enumeratum"
  ).map(_ % Versions.enumeratum)

  private val loggingDeps = Seq(
    "ch.qos.logback" % "logback-classic"
  ).map(_ % Versions.logback)

  private val scalaTest = Seq(
    "org.scalatest" %% "scalatest"
  ).map(_ % Versions.scalaTest % Test)

  private val akkaTest = Seq(
    "com.typesafe.akka" %% "akka-actor-testkit-typed",
    "com.typesafe.akka" %% "akka-stream-testkit"
  ).map(_ % Versions.akka % Test)

  val neatDependencies = {
    akka ++
    enumeratum ++
    loggingDeps ++
    scalaTest ++
    akkaTest
  }
}
