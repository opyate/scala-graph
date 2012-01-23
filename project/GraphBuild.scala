import sbt._
import Keys._

object GraphBuild extends Build {
  lazy val all = Project(
    id = "Graph-all",
    base = file("."),
    settings = defaultSettings ++ Seq(
      name         := "Graph for Scala",
      organization := "scalax.collection.graph",
      version      := "1.3.2"
	),
    aggregate = Seq(core, json/*, constrained*/)
  )
  lazy val core = Project(
    id = "Graph-core",
    base = file("core"),
    settings = defaultSettings ++ Seq(
      name         := "Graph for Scala - Core",
      organization := "scalax.collection.graph",
      version      := "1.3.2"
	)
  )
  lazy val json = Project(
    id = "Graph-json",
    base = file("json"),
    settings = defaultSettings ++ Seq(
	  libraryDependencies ++= Seq(
	    "net.liftweb" %% "lift-json" % "2.4-M4"
      ),
      name         := "Graph for Scala - JSON",
      organization := "scalax.collection.graph.io.json",
      version      := "1.3.2"
	)
  ) dependsOn (core)
  lazy val constrained = Project(
    id = "Graph-constrained",
    base = file("constrained"),
    settings = defaultSettings
  ) dependsOn (core, json)

  lazy val defaultSettings = Defaults.defaultSettings ++ Seq(
    scalaVersion := "2.9.1",
    parallelExecution in Test := false,
    scalacOptions ++= Opts.compile.encoding("Cp1252") :+
                      Opts.compile.deprecation :+
                      Opts.compile.unchecked,
    scalacOptions in (Compile, doc) <++= (name, version) map {
                                         Opts.doc.title(_) ++ Opts.doc.version(_) },
    testOptions in Test := Seq(Tests.Filter(s => s.endsWith("Test"))),
    libraryDependencies ++= Seq(
      "junit" % "junit" % "4.8.2" % "test",
      "org.scalatest" % "scalatest_2.9.0" % "1.6.1" % "test"
    )
  )
}