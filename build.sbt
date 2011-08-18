organization := "edu.bryant.tabu"

name := "tabu"

version := "1.0"

scalaVersion := "2.9.1.RC2"

retrieveManaged := true

testFrameworks += new TestFramework("org.specs2.runner.SpecsFramework")

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "1.6-SNAPSHOT" % "test",
  "net.lag" % "configgy" % "2.0.2"
)

resolvers += "Twitter Repository" at "http://maven.twttr.com"

resolvers += ScalaToolsSnapshots
