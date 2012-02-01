name := "scalan"

organization := "com.github.scalan"

version := "0.2.0"

scalaVersion := "2.10.0-virtualized-SNAPSHOT"

scalacOptions ++= Seq("-unchecked", "-deprecation")

scalacOptions <++= scalaVersion map { version =>
  val Version = """(\d+)\.(\d+)\..*"""r
  val Version(major0, minor0) = version map identity
  val (major, minor) = (major0.toInt, minor0.toInt)
  if (major < 2 || (major == 2 && minor < 10)) 
  	Seq("-Ydependent-method-types")
 	else Nil
}

resolvers ++= Seq(ScalaToolsSnapshots)

libraryDependencies ++= Seq(
  "com.novocode" % "junit-interface" % "0.7" % "test",
  "org.scala-lang" % "scala-compiler" % "2.10.0-virtualized-SNAPSHOT"
)

//Our tests are not threadsafe so disabling parallel execution for now
parallelExecution in Test := false

scalaHome <<= baseDirectory { f =>
  val props = new java.util.Properties()
  IO.load(props, f / "local.properties")
  val x = props.getProperty("scala.virtualized.home")
  if (x == null)
    sys.error("Did you forget to set scala.virtualized.home property in local.properties file?")
  else Some(file(x))
}

//scalaHome := Some(file("../scala-virtualized/build/pack"))

//scalaSource in Compile <<= baseDirectory(_ / "src")

//sourceDirectories in Compile ++= Seq(
//	file("src"),
//        file("samples"))

//sourceDirectories in Compile <<= baseDirectory(base => Seq(
//	base / "src",
//        base / "samples"))

//scalaSource in Test <<= baseDirectory(_ / "src")


//javaOptions ++= Seq("-XX:+CMSPermGenSweepingEnabled",
//                    "-XX:+CMSClassUnloadingEnabled",
//                    "-XX:PermSize=512M",
//                    "-XX:MaxPermSize=1024M",
//                    "-XX:+UseConcMarkSweepGC",
//                    "-Xss1m", "-server")
//javaOptions ++= Seq("-Xmx512m")


