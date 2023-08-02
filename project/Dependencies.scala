import sbt._

object Versions {
  val enumeratum = "1.7.3"
  val scalaTest = "3.2.16"
}

object Dependencies {

  private val enumeratum = Seq(
    "com.beachape" %% "enumeratum"
  ).map(_ % Versions.enumeratum)

  private val scalaTest = Seq(
    "org.scalatest" %% "scalatest"
  ).map(_ % Versions.scalaTest % Test)

  val neatDependencies =
    enumeratum ++
    scalaTest
}
