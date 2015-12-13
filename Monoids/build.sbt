name := "Functional Programming in Scala - Monoids"

version := "1.0"

scalaVersion := "2.11.7"

resolvers ++= Seq("snapshots" at "http://scala-tools.org/repo-snapshots",
  "releases" at "http://scala-tools.org/repo-releases")

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.5" % "test"
