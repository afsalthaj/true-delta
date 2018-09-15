import sbt.CrossVersion

name := "true-delta"

version := "1.0.0"

scalaVersion := "2.12.4"

libraryDependencies ++= Seq(
  "org.typelevel"   %% "cats-core"         % "1.4.0",
  "com.beachape"    %% "enumeratum"        % "1.5.13",
  "com.chuusai"     %% "shapeless"         % "2.3.3",
  "org.specs2"      %% "specs2-scalacheck" % "4.3.4",
  "org.scalacheck"  %% "scalacheck"        % "Version.scalacheck",
  "org.specs2"      %% "specs2-core"       % "4.3.4",
)

fork := true

addCompilerPlugin("org.spire-math" % "kind-projector" % "0.9.6" cross CrossVersion.binary)