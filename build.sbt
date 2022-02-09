import com.jsuereth.sbtpgp.SbtPgp.autoImport._
import sbt.Keys._
import sbt.{Resolver, _}

inThisBuild(List(
  organization := "com.github.stacycurl",
  homepage     := Some(url("https://github.com/stacycurl/jsondbc")),
  licenses     := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
  developers   := List(
    Developer("stacycurl", "Stacy Curl", "stacy.curl@gmail.com", url("https://github.com/stacycurl"))
  ),
  usePgpKeyHex("pimpathon ci"),
  //    moduleName                <<= name("jsondbc-" + _),
  organization              := "com.github.stacycurl",
  scalaVersion              := "2.12.3",
  crossScalaVersions        := Seq("2.12.3"),
  scalacOptions             := Seq("-feature", "-Xfatal-warnings", "-deprecation", "-unchecked", "-target:jvm-1.8"),
  javacOptions              := Seq("-source", "1.8", "-target", "1.8", "-Xlint"),
  maxErrors                 := 1,
  parallelExecution in Test := true,
  resolvers += Resolver.sonatypeRepo("releases"),
  libraryDependencies ++= scalaVersion(dependencies("2.12.3" → List(
    "org.scala-lang"             % "scala-compiler"    % "2.12.3" exclude("org.scala-lang.modules", "scala-xml_2.12"),
    "org.scala-lang"             % "scala-library"     % "2.12.3"    % "test",
    "com.github.julien-truffaut" %% "monocle-core"     % "1.5.0",
    "org.scalaz"                 %% "scalaz-core"      % "7.3.0-M6",

    "com.novocode"               % "junit-interface"   % "0.11"      % "test",
    "com.github.stacycurl"       %% "delta-matchers"   % "1.2.0"     % "test",
    "co.fs2"                     %% "fs2-core"         % "0.10.2"    % "test"
  ))).value
))

lazy val root = (project in file(".")
  aggregate(core, argonaut, circe, spray, json4s)
  settings(
    publish := {},
    publishLocal := {},
    publishArtifact := false
  )
)

lazy val core = (project in file("core")
//    settings(Publishing.settings: _*)
  settings(
    libraryDependencies ++= scalaVersion(dependencies("2.12.3" → List(
      "io.gatling" %% "jsonpath" % "0.6.8"
    ))).value
  )
  settings(
    initialize := {
      val _ = initialize.value
      require(sys.props("java.specification.version") == "1.8", "Java 8 is required for this project.")
    }
  )
)

lazy val argonaut = (project in file("argonaut")
  //    settings(Publishing.settings: _*)
  settings(
    libraryDependencies ++= scalaVersion(dependencies("2.12.3" → List(
      "io.argonaut"                %% "argonaut"         % "6.3.3",
      "io.argonaut"                %% "argonaut-monocle" % "6.3.3",
      "com.github.stacycurl"       %% "delta-argonaut"   % "1.2.0"  % "test"
    ))).value
  )
  dependsOn core % "compile -> compile; test -> test"
)

lazy val spray = (project in file("spray")
  //    settings(Publishing.settings: _*)
  settings(
    libraryDependencies ++= scalaVersion(dependencies("2.12.3" → List(
      "io.spray"   %% "spray-json"  % "1.3.4",
      "org.scalaz" %% "scalaz-core" % "7.3.0-M6"
    ))).value
  )
  dependsOn core % "compile -> compile; test -> test"
)

lazy val circe = (project in file("circe")
  //    settings(Publishing.settings: _*)
  settings(
    libraryDependencies ++= scalaVersion(dependencies("2.12.3" → List(
      "io.circe" %% "circe-core"        % "0.9.3",
      "io.circe" %% "circe-generic"     % "0.9.3",
      "io.circe"  % "circe-optics_2.12" % "0.9.3",
      "io.circe" %% "circe-parser" % "0.9.3" % "test"
    ))).value
  )
  dependsOn core % "compile -> compile; test -> test"
)

lazy val json4s = (project in file("json4s")
  //    settings(Publishing.settings: _*)
  settings(
    libraryDependencies ++= scalaVersion(dependencies("2.12.3" → List(
      "org.json4s" %% "json4s-core"   % "3.6.0",
      "org.json4s" %% "json4s-native" % "3.6.0" % "test"
    ))).value
  )
  dependsOn core % "compile -> compile; test -> test"
)

lazy val play = (project in file("play")
  //    settings(Publishing.settings: _*)
  settings(
    libraryDependencies ++= scalaVersion(dependencies("2.12.3" → List(
      "com.typesafe.play" %% "play-json" % "2.8.1"
    ))).value
  )
  dependsOn core % "compile -> compile; test -> test"
)

def dependencies(modules: (String, List[ModuleID])*)(version: String) = modules.toMap.apply(version)