name := "Weather"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies += "joda-time" % "joda-time" % "2.3"


libraryDependencies ++= Seq(
  "org.specs2" % "specs2-core_2.11" % "3.8.3" % "test"
 )

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
resolvers += "MVN Repo" at "https://mvnrepository.com/artifact"


    