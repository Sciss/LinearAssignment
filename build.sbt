lazy val root = project.in(file("."))
  .settings(
    name                 := "LinearAssignment",
    version              := "0.1.0-SNAPSHOT",
    organization         := "de.sciss",
    scalaVersion         := "2.13.3",
    crossScalaVersions   := Seq("2.13.3", "2.12.12"),
    description          := "Scala implementation of linear assignment problem algorithms such as Hungarian or Kuhn-Munkres and Jonker-Volgenant.",
    homepage             := Some(url(s"https://github.com/Sciss/${name.value}")),
    licenses             := Seq("LGPL v2.1+" -> url("http://www.gnu.org/licenses/lgpl-2.1.txt")),
    scalacOptions       ++= Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xfuture", "-Xlint:-stars-align,_"),
    libraryDependencies ++= Seq(
      "org.scalatest"     %% "scalatest"  % "3.1.2" % Test,
      "com.github.scopt"  %% "scopt"      % "3.7.1" % Test,
      "de.sciss"          %% "fileutil"   % "1.1.4" % Test
    )
  )
