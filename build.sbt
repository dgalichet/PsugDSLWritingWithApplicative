name := "DSL Writing with Applicative"

scalaVersion := "2.11.2"

libraryDependencies ++= Seq( 
    "com.typesafe.play" %% "play-functional" % "2.3.3",
    "org.scala-lang.modules" %% "scala-xml" % "1.0.2",
    "org.specs2" %% "specs2" % "2.4.1" % "test"
)

scalacOptions ++= Seq("-feature")

scalacOptions in Test ++= Seq("-Yrangepos")

resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo)

resolvers += "Typesafe Releases Repository" at "http://repo.typesafe.com/typesafe/releases/"
