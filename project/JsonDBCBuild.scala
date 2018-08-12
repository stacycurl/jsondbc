import sbt.Keys._
import sbt._
//import scoverage.ScoverageKeys._


object JsonDBCBuild extends Build {
  lazy val root = (project in file(".")
    aggregate(core, argonaut, circe)
    settings(commonSettings: _*)
    settings(
      initialize := {
        val _ = initialize.value
        require(sys.props("java.specification.version") == "1.8", "Java 8 is required for this project.")
      },
      publish := (),
      publishLocal := ()
    )
  )

  lazy val core = (project in file("core")
    settings(commonSettings: _*)
    settings(
      initialize := {
        val _ = initialize.value
        require(sys.props("java.specification.version") == "1.8", "Java 8 is required for this project.")
      }
    )
    settings(Publishing.settings: _*)
  )

  lazy val argonaut = (project in file("argonaut")
    settings(commonSettings: _*)
    settings(
      libraryDependencies <++= scalaVersion(dependencies("2.12.3" → List(
        "io.argonaut"                %% "argonaut"         % "6.2.2"    % "provided",
        "io.argonaut"                %% "argonaut-monocle" % "6.2.2"    % "provided",
        "org.scalaz"                 %% "scalaz-core"      % "7.3.0-M6" % "provided"
      )))
    )
    settings(Publishing.settings: _*)
  ).dependsOn(core)

  lazy val circe = (project in file("circe")
    settings(commonSettings: _*)
    settings(
      libraryDependencies <++= scalaVersion(dependencies("2.12.3" → List(
        "io.circe" %% "circe-core"        % "0.9.3",
        "io.circe"  % "circe-optics_2.12" % "0.9.3"
      )))
    )
    settings(Publishing.settings: _*)
  ).dependsOn(core)

  private def commonSettings = Seq(
    organization              := "com.github.stacycurl",
    scalaVersion              := "2.12.3",
    crossScalaVersions        := Seq("2.12.3"),
    scalacOptions             := Seq("-feature", "-Xfatal-warnings", "-deprecation", "-unchecked", "-target:jvm-1.8"),
    javacOptions              := Seq("-source", "1.8", "-target", "1.8", "-Xlint"),
    maxErrors                 := 1,
    parallelExecution in Test := true,
    resolvers += "Stacy Curl's repo" at "http://dl.bintray.com/stacycurl/repo/",
    resolvers += "jcenter" at "http://jcenter.bintray.com",
    libraryDependencies <++= scalaVersion(dependencies("2.12.3" → List(
      "org.scala-lang"             % "scala-compiler"    % "2.12.3" exclude("org.scala-lang.modules", "scala-xml_2.12"),
      "org.scala-lang"             % "scala-library"     % "2.12.3"    % "test",
      "com.github.julien-truffaut" %% "monocle-core"     % "1.5.0"     % "provided",
      "org.scalaz"                 %% "scalaz-core"      % "7.3.0-M6"  % "provided",
      "io.gatling"                 %% "jsonpath"         % "0.6.8"     % "provided",

      "com.novocode"               % "junit-interface"   % "0.11"      % "test",
      "com.github.stacycurl"       %% "delta-matchers"   % "1.1.0"     % "test",
      "co.fs2"                     %% "fs2-core"         % "0.10.2"    % "test"
    )))
  )

  private def dependencies(modules: (String, List[ModuleID])*)(version: String) = modules.toMap.apply(version)
}
