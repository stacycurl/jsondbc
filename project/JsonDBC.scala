import com.jsuereth.sbtpgp.SbtPgp.autoImport.usePgpKeyHex
import sbt.Keys.{name, _}
import sbt.{Def, _}

object JsonDBC {
  def dependantProject(core: Project, projectName: String)(project: Project): Project = (project in file(projectName)
    dependsOn core % "compile -> compile; test -> test"
    settings(commonSettings: _*)
    settings(name := s"jsondbc-$projectName")
  )

  lazy val commonSettings: Seq[Def.Setting[_]] = Seq(
    homepage     := Some(url("https://github.com/stacycurl/jsondbc")),
    licenses     := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers   := List(
      Developer("stacycurl", "Stacy Curl", "stacy.curl@gmail.com", url("https://github.com/stacycurl"))
    ),
    usePgpKeyHex("pimpathon ci"),
    organization             := "com.github.stacycurl",
    scalaVersion              := "2.12.3",
    crossScalaVersions        := Seq("2.12.3"),
    maxErrors                := 1,
    Test / parallelExecution := true,
    scalacOptions             := Seq("-feature", "-Xfatal-warnings", "-deprecation", "-unchecked", "-target:jvm-1.8"),
    javacOptions              := Seq("-source", "1.8", "-target", "1.8", "-Xlint"),
    resolvers += Resolver.sonatypeRepo("releases"),
    libraryDependencies ++= List(
      "org.scala-lang"             % "scala-compiler"    % "2.12.3" exclude("org.scala-lang.modules", "scala-xml_2.12"),
      "org.scala-lang"             % "scala-library"     % "2.12.3"    % "test",
      "com.github.julien-truffaut" %% "monocle-core"     % "1.5.0",
      "org.scalaz"                 %% "scalaz-core"      % "7.3.0-M6",

      "com.novocode"               % "junit-interface"   % "0.11"      % "test",
      "com.github.stacycurl"       %% "delta-matchers"   % "1.2.0"     % "test",
      "co.fs2"                     %% "fs2-core"         % "0.10.2"    % "test"
    ),
    initialize := {
      val _ = initialize.value
      require(sys.props("java.specification.version") == "1.8", "Java 8 is required for this project.")
    }
  )
}
