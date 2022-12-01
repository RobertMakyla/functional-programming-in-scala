name := "functional-programming-in-scala"

version := "1.0"

scalaVersion := "2.13.8"


libraryDependencies ++= {
  Seq(
    "org.scalatest" %% "scalatest" % "3.2.7" % "test",
    "org.scalacheck" %% "scalacheck" % "1.15.4" % "test"
  )
}