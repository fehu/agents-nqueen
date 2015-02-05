lazy val commonSettings = Seq(
  organization := "feh.tec",
  scalaVersion := "2.11.5"
)

lazy val root = (project in file("."))
  .settings(commonSettings: _*)
  .aggregate(comm, agent)

lazy val comm = RootProject(file( "../../negotiation" ))

lazy val agent = (project in file("agent"))
  .settings(commonSettings: _*)
  .dependsOn(comm)

publishArtifact := false
