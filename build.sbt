enablePlugins(ScalaJSPlugin)
scalaVersion in ThisBuild := "2.12.4"
// scalaVersion := "2.12.2"


lazy val root = project.in(file(".")).
  aggregate(eulerJS, eulerJVM).
  settings(
    publish := {},
    publishLocal := {},
    exportJars := true
  )

lazy val euler = crossProject.in(file(".")).
  settings(
    name := "euler",
    version := "0.5.3-SNAPSHOT",
    organization := "skac",
    libraryDependencies += "org.scalaz" %%% "scalaz-core" % "7.2.13",
    libraryDependencies += "com.lihaoyi" %%% "utest" % "0.6.3" % "test",
    testFrameworks += new TestFramework("utest.runner.Framework")
  ).
  jvmSettings(
    libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.6",
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
    // Add JVM-specific settings here
  ).
  jsSettings(
    // Add JS-specific settings here
  )

lazy val eulerJVM = euler.jvm
lazy val eulerJS = euler.js
