name := "functional-programming-in-scala"

version := "1.0"

scalaVersion := "2.12.0"

libraryDependencies ++= {
  val scalaTestV       = "3.0.1"

  Seq(
    "org.scalatest"  %% "scalatest"  % scalaTestV % "test"
  )
}