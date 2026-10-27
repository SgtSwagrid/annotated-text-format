import IdeSettings.packagePrefix
import sbt._
import sbt.Keys._
import sbtunidoc.BaseUnidocPlugin.autoImport.*
import sbtunidoc.ScalaUnidocPlugin

val scala3 = "3.9.0"

val projectRoot = "com.alecdorrington.annotatedtextformat"

ThisBuild / scalaVersion := scala3

ThisBuild / scalacOptions ++= Seq(
  "-explain",
  "-explain-types",
  "-explain-cyclic",
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Wunused:all",
)

// The DOM module's tests give their documents a DOM through jsdom, from npm.
Npm.installation

/**
  * Settings of the JVM's side of each matrix: a workaround for an sbt 2.0.x
  * classloader bug where in-process test runs can't see Test-scoped
  * dependencies (NoClassDefFoundError: munit/FunSuite).
  */
val onJvm = Seq(Test / fork := true)

// Every project is named after the library so as not to clash with the
// projects of a build that includes this one by reference. A matrix resolves
// sources against the working directory, which is not this build's base when
// another build includes it by reference, so each names its own.

/** The format itself: annotated text, its validation, codecs and JSON Schema. */
lazy val `annotated-text-format` = projectMatrix
  .in(file("."))
  .settings(
    packagePrefix   := projectRoot,
    sourceDirectory := (ThisBuild / baseDirectory).value / "src",
    Dependencies.circe,
    Dependencies.munit,
  )
  .jvmPlatform(
    scalaVersions = Seq(scala3),
    settings = onJvm,
  )
  .jsPlatform(scalaVersions = Seq(scala3))

/**
  * The interface to large language models: annotations quoted as a model writes
  * them, and anchored to the text.
  */
lazy val `annotated-text-format-llm` = projectMatrix
  .in(file("llm"))
  .dependsOn(`annotated-text-format`)
  .settings(
    packagePrefix   := s"$projectRoot.llm",
    sourceDirectory := (ThisBuild / baseDirectory).value / "llm" / "src",
    Dependencies.circe,
    Dependencies.munit,
  )
  .jvmPlatform(
    scalaVersions = Seq(scala3),
    settings = onJvm,
  )
  .jsPlatform(scalaVersions = Seq(scala3))

/** Rendering in the DOM: annotations highlighted in a page's own text. */
lazy val `annotated-text-format-dom` = projectMatrix
  .in(file("dom"))
  .dependsOn(`annotated-text-format`)
  .settings(
    packagePrefix   := s"$projectRoot.dom",
    sourceDirectory := (ThisBuild / baseDirectory).value / "dom" / "src",
    Dependencies.scalajsDom,
    Dependencies.munit,
    Npm.beforeTests,
  )
  .jsPlatform(scalaVersions = Seq(scala3))

lazy val `annotated-text-format-root` = project
  .in(file("."))
  .enablePlugins(ScalaUnidocPlugin)
  .aggregate(
    `annotated-text-format`.projectRefs ++
      `annotated-text-format-llm`.projectRefs ++
      `annotated-text-format-dom`.projectRefs *,
  )
  .settings(
    publish / skip := true,

    // The root shares the core's directory, so must not build its sources.
    Compile / unmanagedSourceDirectories := Nil,
    Test / unmanagedSourceDirectories    := Nil,

    // Only the JS side has every module, and the JVM's would document the
    // same sources again.
    ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(
      `annotated-text-format`.js(scala3),
      `annotated-text-format-llm`.js(scala3),
      `annotated-text-format-dom`.js(scala3),
    ),
    ScalaUnidoc / unidoc / scalacOptions ++=
      Seq("-project", "Annotated Text Format"),
  )
