name := "sandbox"

mainClass in (Compile, packageBin) := Some("sandbox.Main")

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % "2.11.6"
)

