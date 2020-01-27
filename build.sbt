import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

val sharedSettings = Seq(
  name := "euler",
  version := "0.10.0",
  organization := "skac112",
  scalaVersion := "2.12.8",
  libraryDependencies += "org.typelevel" %% "cats-core" % "1.2.0",
  libraryDependencies += "com.lihaoyi" %%% "utest" % "0.6.3" % "test",
  testFrameworks += new TestFramework("utest.runner.Framework")
)

val jvmSettings = Seq(
  libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.6",
  libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)

val jsSettings = Seq(
)

lazy val euler = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("."))
  .settings(sharedSettings)
  .jsSettings(jsSettings) // defined in sbt-scalajs-crossproject
  .jvmSettings(jvmSettings)

lazy val eulerJVM = euler.jvm
lazy val eulerJS = euler.js

ThisBuild / organization := "com.github.skac112"
ThisBuild / organizationName := "skac112"
ThisBuild / organizationHomepage := Some(url("https://github.com/skac112/euler"))

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/skac112/euler"),
    "scm:git:git://github.com/skac112/euler.git"
  )
)
ThisBuild / developers := List(
  Developer(
    id    = "skac112",
    name  = "Sławek Kacprzykowski",
    email = "skac@poczta.fm",
    url   = url("https://github.com/skac112")
  )
)

ThisBuild / description := "Scala library for graphs (nodes connected by edges)."
ThisBuild / licenses := List("Apache 2" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt"))
ThisBuild / homepage := Some(url("https://github.com/skac112/euler"))

// Remove all additional repository other than Maven Central from POM
ThisBuild / pomIncludeRepository := { _ => false }
ThisBuild / publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}
ThisBuild / publishMavenStyle := true