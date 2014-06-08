name := "root"

version := "1.0"

scalaVersion := "2.10.3"

lazy val base = project.in(file(".")).aggregate(core, dambreak)

lazy val core = project in file("core")

lazy val dambreak = project.in(file("dambreak")).dependsOn(core)
