name := "Scala2"

version := "1.0"


scalaVersion := "2.11.2"

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies ++= Seq(
  "com.github.marklister" %% "product-collections" % "1.4.2",
  "xyz.wiedenhoeft" %% "scalacrypt" % "0.5-SNAPSHOT",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test")
    
