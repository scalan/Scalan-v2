name := "scalan"

organization := "com.github.scalan"

version := "0.2"

scalaVersion := "2.10.0-virtualized-SNAPSHOT"

scalaHome := Some(file("../scala-virtualized/build/pack"))

//scalaSource in Compile <<= baseDirectory(_ / "src")

//sourceDirectories in Compile ++= Seq(
//	file("src"),
//        file("samples"))

sourceDirectories in Compile <<= baseDirectory(base => Seq(
	base / "src",
        base / "samples"))

scalaSource in Test <<= baseDirectory(_ / "src")


//javaOptions ++= Seq("-XX:+CMSPermGenSweepingEnabled",
//                    "-XX:+CMSClassUnloadingEnabled",
//                    "-XX:PermSize=512M",
//                    "-XX:MaxPermSize=1024M",
//                    "-XX:+UseConcMarkSweepGC",
//                    "-Xss1m", "-server")

javaOptions ++= Seq("-Xmx512m")

resolvers ++= Seq(ScalaToolsSnapshots)

libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.10.0-virtualized-SNAPSHOT"

scalacOptions += "-deprecation"

//Our tests are not threadsafe so disabling parallel execution for now
parallelExecution in Test := false

