import JsonDBC._
import sbt.Keys._
import sbt._

lazy val root = (project in file(".")
  aggregate(core, argonaut, circe, spray, json4s)
  settings(commonSettings: _*)
  settings(
    publish := {},
    publishLocal := {},
    publishArtifact := false
  )
)

lazy val core = (project in file("core")
  settings(commonSettings: _*)
  settings(
    name := "jsondbc-core",
    libraryDependencies ++= List(
      "io.gatling"      %% "jsonpath"    % "0.6.8",
      "org.reflections"  % "reflections" % "0.9.11",
    )
  )
)

lazy val argonaut = (project configure dependantProject(core, "argonaut")
  settings(
    libraryDependencies ++= List(
      "io.argonaut"                %% "argonaut"         % "6.3.3",
      "io.argonaut"                %% "argonaut-monocle" % "6.3.3",
      "com.github.stacycurl"       %% "delta-argonaut"   % "1.2.0"  % "test"
    )
  )
)

lazy val spray = (project configure dependantProject(core, "spray")
  settings(
    libraryDependencies ++= List(
      "io.spray"   %% "spray-json"  % "1.3.4",
      "org.scalaz" %% "scalaz-core" % "7.3.0-M6"
    )
  )
)

lazy val circe = (project configure dependantProject(core, "circe")
  settings(
    libraryDependencies ++= List(
      "io.circe" %% "circe-core"        % "0.9.3",
      "io.circe" %% "circe-generic"     % "0.9.3",
      "io.circe"  % "circe-optics_2.12" % "0.9.3",
      "io.circe" %% "circe-parser" % "0.9.3" % "test"
    )
  )
)

lazy val json4s = (project configure dependantProject(core, "json4s")
  settings(
    libraryDependencies ++= List(
      "org.json4s" %% "json4s-core"   % "3.6.0",
      "org.json4s" %% "json4s-native" % "3.6.0" % "test"
    )
  )
)

lazy val play = (project configure dependantProject(core, "play")
  settings(
    libraryDependencies ++= List(
      "com.typesafe.play" %% "play-json" % "2.8.1"
    )
  )
)