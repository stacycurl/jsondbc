import sbt.Keys._
import sbt._
//import scoverage.ScoverageKeys._


object JsonDBCBuild extends Build {
  lazy val jsondbc = (project in file(".")
    settings(
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

        "io.argonaut"                %% "argonaut"         % "6.2.2"   % "provided",
        "io.argonaut"                %% "argonaut-monocle" % "6.2.2"   % "provided",
        "org.scalaz"                 %% "scalaz-core"      % "7.3.0-M6"  % "provided",

        "io.gatling"                 %% "jsonpath"         % "0.6.8"     % "provided",

        "io.circe"                   %% "circe-core"       % "0.9.3",
        "io.circe"                   % "circe-optics_2.12" % "0.9.3",

        "com.novocode"               % "junit-interface"   % "0.11"      % "test",
        "com.github.stacycurl"       %% "delta-matchers"   % "1.1.0"     % "test",
        "co.fs2"                     %% "fs2-core"         % "0.10.2"    % "test"
      ))),
      initialize := {
        val _ = initialize.value
        require(sys.props("java.specification.version") == "1.8", "Java 8 is required for this project.")
      }
      //    coverageEnabled := true,
      //    coverageMinimum := 100,
      //    coverageHighlighting := true,
      //    coverageFailOnMinimum := true
    )
    settings(Publishing.settings: _*)
    settings addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
  )

  private def dependencies(modules: (String, List[ModuleID])*)(version: String) = modules.toMap.apply(version)
}
