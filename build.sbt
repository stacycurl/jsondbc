import JsonDBC._
import sbt.Keys._
import sbt._

lazy val jsondbc = (project in file(".")
  aggregate(core, scalazOptics, catsOptics, argonaut, circe, spray, json4s)
  settings(commonSettings: _*)
  settings(noPublish: _*)
)

lazy val core = (project in file("core")
  settings(commonSettings: _*)
  settings(
    name := "jsondbc-core",
    libraryDependencies ++= List(
      "io.gatling"      %% "jsonpath"    % "0.6.8",
      "org.reflections"  % "reflections" % "0.9.11"
    )
  )
)

lazy val scalazOptics = (project configure dependantProject("scalaz-optics", core)
  settings(commonSettings: _*)
  settings(
    libraryDependencies ++= List(
      "io.argonaut"                %% "argonaut-monocle" % "6.3.3",
      "com.github.julien-truffaut" %% "monocle-core"     % "1.7.3",
      "org.scalaz"                 %% "scalaz-core"      % "7.3.2"
    )
  )
)

lazy val catsOptics = (project configure dependantProject("cats-optics", core)
  settings(commonSettings: _*)
  settings(
    libraryDependencies ++= List(
      "com.github.julien-truffaut" %% "monocle-core"     % "1.5.1-cats",
      "org.typelevel"              %% "cats-core"        % "1.4.0"
    )
  )
)

lazy val argonaut = (project configure dependantProject("argonaut", core, scalazOptics)
  settings(
    libraryDependencies ++= List(
      "io.argonaut"                %% "argonaut"         % "6.3.3",
      "com.github.stacycurl"       %% "delta-argonaut"   % "1.2.0"  % "test"
    )
  )
)

lazy val spray = (project configure dependantProject("spray", core, scalazOptics)
  settings(
    libraryDependencies ++= List(
      "io.spray"   %% "spray-json"  % "1.3.4"
    )
  )
)

lazy val circe = (project configure dependantProject("circe", core, catsOptics)
  settings(
    libraryDependencies ++= List(
      "io.circe" %% "circe-core"        % "0.10.0",
      "io.circe" %% "circe-generic"     % "0.10.0",
      "io.circe"  % "circe-optics_2.12" % "0.10.0",
      "io.circe" %% "circe-parser" % "0.10.0" % "test"
    )
  )
)

lazy val json4s = (project configure dependantProject("json4s", core, scalazOptics)
  settings(
    libraryDependencies ++= List(
      "org.json4s" %% "json4s-core"   % "3.6.0",
      "org.json4s" %% "json4s-native" % "3.6.0" % "test"
    )
  )
)

lazy val play = (project configure dependantProject("play", core, scalazOptics)
  settings(
    libraryDependencies ++= List(
      "com.typesafe.play" %% "play-json" % "2.8.1"
    )
  )
)