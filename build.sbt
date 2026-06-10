
lazy val root = (project in file("."))
  .settings(
    name         := "hp-kimi-scala",
    organization := "net.entelijan.kimihp",
    scalaVersion := "2.12.21",
    version      := "1.0-SNAPSHOT",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.20" % "test",
    libraryDependencies += "org.planet42" %% "laika-core" % "0.19.5",
    libraryDependencies += "org.apache.poi" % "poi-ooxml" % "5.5.1",
    libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.10.7",
)
