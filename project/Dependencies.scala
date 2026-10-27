import sbt.*
import sbt.Keys.*

/** The libraries that the modules depend on. */
object Dependencies:

  object V:

    val circe      = "0.14.17"
    val scalajsDom = "2.8.1"
    val munit      = "1.3.6"

  lazy val circe = libraryDependencies ++= Seq(
    "io.circe" %% "circe-core"   % V.circe,
    "io.circe" %% "circe-parser" % V.circe % Test,
  )

  lazy val scalajsDom = libraryDependencies +=
    "org.scala-js" %% "scalajs-dom" % V.scalajsDom

  lazy val munit = libraryDependencies +=
    "org.scalameta" %% "munit" % V.munit % Test
