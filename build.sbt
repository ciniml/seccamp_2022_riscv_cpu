// See README.md for license details.

ThisBuild / scalaVersion     := "2.13.12"
ThisBuild / version          := "0.1.0"
ThisBuild / organization     := "%ORGANIZATION%"

lazy val commonSettings = Seq (
  libraryDependencies ++= Seq(
    "org.chipsalliance" %% "chisel" % "6.2.0",
    "org.scalatest" %% "scalatest" % "3.2.16" % "test",
    "edu.berkeley.cs" %% "chiseltest" % "6.0.0"
  ),
  scalacOptions ++= Seq(
    "-language:reflectiveCalls",
    "-deprecation",
    "-feature",
    "-Xcheckinit",
    "-Ymacro-annotations"
  ),
  addCompilerPlugin("org.chipsalliance" % "chisel-plugin" % "6.2.0" cross CrossVersion.full),
)

lazy val root = (project in file(".")).
  settings(
    commonSettings,
    name := "seccamp_riscv_cpu"
  )