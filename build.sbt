val V = new {
  val cats = "2.0.0-M4"
}

lazy val root = project
  .in(file("."))
  .settings(
    name := "grammarize",
    version := "1.0",
    scalaVersion := "2.13.0",
    libraryDependencies ++= Seq(
      "org.typelevel"     %% "cats-core" % V.cats,
      "org.typelevel"     %% "cats-free" % V.cats,
      "com.github.mifmif" % "generex"    % "1.0.2"
    )
  )
