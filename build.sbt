
lazy val root = (project in file("."))
  .settings(
    name         := "hp-kimi-scala",
    organization := "net.entelijan.kimihp",
    scalaVersion := "2.12.8",
    version      := "1.0-SNAPSHOT",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test",
    libraryDependencies += "org.planet42" %% "laika-core" % "0.7.5"
)
