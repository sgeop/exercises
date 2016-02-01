scalaVersion := "2.11.4"

name := "slot-machine"

mainClass in (Compile, packageBin) := Some("langton.Game")
