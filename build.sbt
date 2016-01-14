name := "inverse-macro"

val defaultSetting = Seq(
  organization := "jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi",
  version := "1.0-SNAPSHOT",
  scalaVersion := "2.12.0-M3",
  javacOptions ++= Seq("-source", "1.8", "-target", "1.8"),
  scalacOptions ++= Seq("-feature", "-deprecation", "-Yopt:l:classpath", "-unchecked", "-explaintypes"),
  // additional options
  scalacOptions ++= Seq("-Xlint", "-Yinline-warnings"),
  resolvers += Resolver.sonatypeRepo("releases"),
  resolvers += "hiroshi-cl" at "https://hiroshi-cl.github.io/sbt-repos/",
  publishMavenStyle := true,
  publishArtifact := false,
  publishTo := Some(Resolver.file("file",  new File("target/repo"))),
  parallelExecution in Test := false,
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-compiler" % scalaVersion.value,
    "org.scala-lang" % "scala-library" % scalaVersion.value,
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    "org.scalatest" %% "scalatest" % "2.2.5-M3" % "test",
    "org.scalacheck" %% "scalacheck" % "1.12.5" % "test",
    "junit" % "junit" % "4.12" % "test"
  )
)

lazy val root = project.in(file(".")).
  aggregate(inverse_macros, inverse_macros_tests, inverse_macros_libraries, paradise_tests).
  settings(defaultSetting: _*)

lazy val inverse_macros = project.
  settings(defaultSetting: _*).
  settings(addCompilerPlugin("jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi" % "paradise" % "2.1.0" cross CrossVersion.full)).
  settings(publishArtifact in Compile := true)

lazy val inverse_macros_libraries = project.dependsOn(inverse_macros).
  settings(defaultSetting: _*).
  settings(addCompilerPlugin("jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi" % "paradise" % "2.1.0" cross CrossVersion.full)).
  settings(publishArtifact in Compile := true)

lazy val inverse_macros_tests = project.dependsOn(inverse_macros).
  settings(defaultSetting: _*).
  settings(addCompilerPlugin("jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi" % "paradise" % "2.1.0" cross CrossVersion.full))

// requires special settings
lazy val paradise_tests = project.dependsOn(inverse_macros).
  settings(defaultSetting ++ Seq(
    addCompilerPlugin("jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi" % "paradise" % "2.1.0" cross CrossVersion.full),
    libraryDependencies += "jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi" % "paradise" % "2.1.0" cross CrossVersion.full, // convert to File
    unmanagedSourceDirectories in Test <<= (scalaSource in Test) { (root: File) =>
      val (anns :: Nil, others) = root.listFiles.toList.partition(_.getName == "annotations")
      val (negAnns, otherAnns) = anns.listFiles.toList.partition(_.getName == "neg")
      System.setProperty("sbt.paths.tests.scaladoc", anns.listFiles.toList.filter(_.getName == "scaladoc").head.getAbsolutePath)
      otherAnns ++ others
    },
    fullClasspath in Test := {
      val testcp = (fullClasspath in Test).value.files.map(_.getAbsolutePath).mkString(java.io.File.pathSeparatorChar.toString)
      sys.props("sbt.paths.tests.classpath") = testcp
      (fullClasspath in Test).value
    },
    dependencyClasspath in Compile := {
      (dependencyClasspath in Compile).value.
        filter(_.metadata.get(moduleID.key).get.toString().contains("paradise")).
        foreach(f => System.setProperty("sbt.paths.plugin.jar", f.data.getAbsolutePath)) // required for some test cases
      (dependencyClasspath in Compile).value
    }
  ): _*)

lazy val sandbox = project.dependsOn(inverse_macros, inverse_macros_libraries).
  settings(defaultSetting: _*).
  settings(addCompilerPlugin("jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi" % "paradise" % "2.1.0" cross CrossVersion.full))

lazy val debug = project.dependsOn(inverse_macros, inverse_macros_libraries).
  settings(defaultSetting: _*).
  settings(scalacOptions ++= Seq("-Xprint:namer,typer", "-Yshow-syms", "-Ystop-after:typer")).
  settings(addCompilerPlugin("jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi" % "paradise" % "2.1.0" cross CrossVersion.full))
