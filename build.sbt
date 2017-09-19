name := "functional-programming-in-scala"

version := "1.0"

scalaVersion := "2.12.0"


libraryDependencies ++= {
  val scalaTestV       = "3.0.1"
  val scalaCheckVersion = "1.13.4"

  Seq(
    "org.scalatest"  %% "scalatest"  % scalaTestV % "test",
    "org.scalacheck" %% "scalacheck" % scalaCheckVersion % "test",
    "net.codingwell" %% "scala-guice" % "4.1.0"
  )
}