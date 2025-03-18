
lazy val root = (project in file("."))
  .settings(
    name         := "hp-kimi-scala",
    organization := "net.entelijan.kimihp",
    scalaVersion := "2.12.15",
    version      := "1.0-SNAPSHOT",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test",
    libraryDependencies += "org.planet42" %% "laika-core" % "0.7.5"
)
