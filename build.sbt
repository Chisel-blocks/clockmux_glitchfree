import scala.sys.process._
// OBS: sbt._ has also process. Importing scala.sys.process
// and explicitly using it ensures the correct operation

ThisBuild / scalaVersion := "2.13.8"
ThisBuild / version      := scala.sys.process.Process("git rev-parse --short HEAD").!!.mkString.replaceAll("\\s", "")+"-SNAPSHOT"
ThisBuild / organization := "Chisel-blocks"

// Last two numbers must be the same
val chiselVersion = "3.5.4"
val chiselTestVersion = "0.5.4"

val breezeVersion = "2.0"
val dspVersion = "1.5.6"

lazy val clockmux = (project in file("."))
  .settings(
    name := "clockmux_glitchfree",
    libraryDependencies ++= Seq(
      "edu.berkeley.cs" %% "chisel3" % chiselVersion,
      "edu.berkeley.cs" %% "chiseltest" % chiselTestVersion,
      "edu.berkeley.cs" %% "dsptools" % dspVersion,
      "org.scalanlp" %% "breeze" % breezeVersion,
      "org.scalanlp" %% "breeze-natives" % breezeVersion,
      "org.scalanlp" %% "breeze-viz" % breezeVersion
    ),
    scalacOptions ++= Seq(
      "-language:reflectiveCalls",
      "-deprecation",
      "-feature",
      "-Xcheckinit",
      "-P:chiselplugin:genBundleElements",
    ),
    addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % chiselVersion cross CrossVersion.full),
  )

// Parse the version of a submodle from the git submodule status
// for those modules not version controlled by Maven or equivalent
def gitSubmoduleHashSnapshotVersion(submod: String): String = {
    val shellcommand =  "git submodule status | grep %s | awk '{print substr($1,0,7)}'".format(submod)
    scala.sys.process.Process(Seq("/bin/sh", "-c", shellcommand )).!!.mkString.replaceAll("\\s", "")+"-SNAPSHOT"
}

// Put your git-version controlled snapshots here
//libraryDependencies += "Chisel-blocks" %% "someblock" % gitSubmoduleHashSnapshotVersion("someblock")


