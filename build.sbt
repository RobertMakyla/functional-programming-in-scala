name := "functional-programming-in-scala"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies ++= {
  val scalaTestV       = "2.2.6"

  Seq(
    "org.scalatest"     %% "scalatest"                         % scalaTestV % "test"
  )
}