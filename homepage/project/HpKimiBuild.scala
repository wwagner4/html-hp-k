import sbt._
import Keys._

object HpKimiBuild extends Build {

  // Constant values
  object D {

    val version = "1.0-SNAPSHOT"

    val scalaVersion = "2.10.6"
  }

  // Settings
  object S {

    lazy val settings =
        Seq(
          scalacOptions += "", // -feature
          version := D.version,
          scalaVersion := D.scalaVersion,
          organization := "net.entelijan.kimihp",
		      libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.5" % "test",
          libraryDependencies += "org.planet42" %% "laika-core" % "0.5.0")
  }

  // Project definitions
  lazy val root = Project(id = "hp-kimi-scala",
    base = file("."), //
    settings = S.settings)
}

