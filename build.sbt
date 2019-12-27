import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

val sharedSettings = Seq(
  name := "euler",
  version := "0.9.0-SNAPSHOT",
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

//lazy val euler = crossProject.in(file(".")).
//  settings(
//    name := "euler",
//    version := "0.8.0-SNAPSHOT",
//    organization := "skac112",
//    scalaVersion := "2.12.4",
////    libraryDependencies += "org.scalaz" %%% "scalaz-core" % "7.2.13",
//    libraryDependencies += "org.typelevel" %% "cats-core" % "1.2.0",
//    libraryDependencies += "com.lihaoyi" %%% "utest" % "0.6.3" % "test",
//    testFrameworks += new TestFramework("utest.runner.Framework")
//  ).
//  jvmSettings(
//    libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.6",
//    libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1",
//    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
//    // Add JVM-specific settings here
//  ).
//  jsSettings(
//    // Add JS-specific settings here
//  )
//
//lazy val eulerJVM = euler.jvm
//lazy val eulerJS = euler.js
//
