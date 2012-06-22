name := "MongoChess"
 
version := "0.1"
 
scalaVersion := "2.9.1"

resolvers += "repo.novus snaps" at "http://repo.novus.com/snapshots/"

resolvers += "scala-tools" at "http://scala-tools.org/repo-releases/" 
 
libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "1.6.1" % "test",
  "com.novus" %% "salat-core" % "0.0.8-SNAPSHOT"
)