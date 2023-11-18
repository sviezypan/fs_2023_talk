import Dependencies._

ThisBuild / scalaVersion := "2.13.12"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "fs_2023_talk",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % "2.0.15",
      "dev.zio" %% "zio-schema" % "0.4.15",
      "dev.zio" %% "zio-schema-derivation" % "0.4.15",
      "dev.zio" %% "zio-schema-avro" % "0.4.15",
      "dev.zio" %% "zio-schema-bson" % "0.4.15",
      "dev.zio" %% "zio-schema-json" % "0.4.15",
      "dev.zio" %% "zio-schema-msg-pack" % "0.4.15",
      "dev.zio" %% "zio-schema-protobuf" % "0.4.15",
      "dev.zio" %% "zio-schema-thrift" % "0.4.15",
      "dev.zio" %% "zio-schema-zio-test" % "0.4.15",
      "dev.zio" %% "zio-test" % "2.0.15" % Test,
      "dev.zio" %% "zio-test-sbt" % "2.0.15" % Test
    )
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
