import sbt._
import Keys._

object GraphBuild extends Build {
  lazy val all = Project(
    id = "Graph-all",
    base = file("."),
    settings = defaultSettings ++ Seq(
      name      := "Graph for Scala",
      version   := Version.all,
      publishTo := None
	  ),
    aggregate = Seq(core, json, constrained)
  )
  lazy val core = Project(
    id = "Graph-core",
    base = file("core"),
    settings = defaultSettings ++ Seq(
      name      := "Graph Core",
      version   := Version.core
	  )
  )
  lazy val json = Project(
    id = "Graph-json",
    base = file("json"),
    settings = defaultSettings ++ Seq(
      name      := "Graph JSON",
      version   := Version.json,
	    libraryDependencies ++= Seq(
	      "net.liftweb" % "lift-json_2.9.1" % "2.4"
      )
	)
  ) dependsOn (core)
  lazy val constrained = Project(
    id = "Graph-constrained",
    base = file("constrained"),
    settings = defaultSettings ++ Seq(
      name      := "Graph Constrained",
      version   := Version.constrained
	)
  ) dependsOn (core % "compile->compile;test->test", json)

  lazy val defaultSettings = Defaults.defaultSettings ++ Seq(
    scalaVersion := "2.9.2",
    organization := "com.assembla.scala-incubator",
    parallelExecution in Test := false,
    scalacOptions ++= Opts.compile.encoding("Cp1252") :+
                      Opts.compile.deprecation :+
                      Opts.compile.unchecked,
    scalacOptions in (Compile, doc) <++= (name, version) map {
      Opts.doc.title(_) ++ Opts.doc.version(_)
    },
    // unmanagedClasspath fixes issue https://github.com/harrah/xsbt/issues/85
    unmanagedClasspath in Compile += Attributed.blank(new java.io.File("dummy")),
    testOptions in Test := Seq(Tests.Filter(s => s.endsWith("Test"))),
    libraryDependencies ++= Seq(
      "junit" % "junit" % "4.8.2" % "test",
      "org.scalatest" % "scalatest_2.9.1" % "1.7.1" % "test"
    )
  ) ++ GraphSonatype.settings
}