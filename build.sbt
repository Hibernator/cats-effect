name := "cats-effect"
version := "0.1.0"
scalaVersion := "3.7.1"
libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-effect" % "3.6.3"
  )
  
scalacOptions ++= Seq(
  "-language:higherKinds",
  "-rewrite",
//  "-Wunused:all",
  "--deprecation",
  "--explain",
  "--feature",
//  "-Werror"
)

semanticdbEnabled := true
semanticdbVersion := scalafixSemanticdb.revision
scalafixOnCompile := true

