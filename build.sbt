name := "inverse-macro"

val defaultSetting = Seq(
  organization := "jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi",
  version      := "1.0-SNAPSHOT",
  scalaVersion := "2.11.7",
  javacOptions  ++= Seq("-source", "1.8", "-target", "1.8"),
  scalacOptions ++= Seq("-feature", "-deprecation", "-optimise", "-unchecked", "-explaintypes"),
  // additional options
  scalacOptions ++= Seq("-Xlint", "-Yinline-warnings"),
  resolvers += Resolver.sonatypeRepo("releases"),
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-compiler" % scalaVersion.value,
    "org.scala-lang" % "scala-library" % scalaVersion.value,
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    "org.scala-lang.plugins" %% "scala-continuations-library" % "1.0.2",
    "org.scalatest" %% "scalatest" % "2.2.4" % "test",
    "org.scalacheck" %% "scalacheck" % "1.12.4" % "test",
    "junit" % "junit" % "4.12" % "test"
  )
)

lazy val root = project.in(file(".")).
  aggregate(inverse_macros, inverse_macros_tests, inverse_macros_libraries, paradise_tests, experiments, experiments_cps).
  settings(defaultSetting: _*)

lazy val inverse_macros = project.settings(
 defaultSetting :+
 addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full): _*)

// plugin の方で先に sbt publishLocal を実行しておく
lazy val inverse_macros_libraries = project.dependsOn(inverse_macros).
  settings(defaultSetting: _*).
  settings(addCompilerPlugin("jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi" % "paradise" % "2.1.0-SNAPSHOT" cross CrossVersion.full))


lazy val inverse_macros_tests = project.dependsOn(inverse_macros).
  settings(defaultSetting: _*).
  settings(addCompilerPlugin("jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi" % "paradise" % "2.1.0-SNAPSHOT" cross CrossVersion.full))

// 諸事情で特殊な設定が必要
lazy val paradise_tests = project.dependsOn(inverse_macros).
  settings(defaultSetting ++ Seq(
    addCompilerPlugin("jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi" % "paradise" % "2.1.0-SNAPSHOT" cross CrossVersion.full),
    libraryDependencies += "jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi" % "paradise" % "2.1.0-SNAPSHOT" cross CrossVersion.full, // File に変換するため
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
        foreach(f => System.setProperty("sbt.paths.plugin.jar", f.data.getAbsolutePath)) // これをセットしておかないといくつかのテストケースで落ちる
      (dependencyClasspath in Compile).value
    }
): _*)

lazy val sandbox = project.dependsOn(inverse_macros, inverse_macros_libraries).
  settings(defaultSetting: _*).
  settings(addCompilerPlugin("jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi" % "paradise" % "2.1.0-SNAPSHOT" cross CrossVersion.full))

lazy val experiments_library = project.settings(defaultSetting: _*)

lazy val experiments = project.dependsOn(inverse_macros, inverse_macros_libraries, experiments_library).
  settings(defaultSetting: _*).
  settings(
    addCompilerPlugin("jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi" % "paradise" % "2.1.0-SNAPSHOT" cross CrossVersion.full),
    assemblyJarName in assembly := "experiments.jar",
    mainClass in assembly := Some("inverse_macros.continuations.MicroBench")
  )

lazy val experiments_cps = project.dependsOn(experiments_library).
  settings(defaultSetting: _*).
  settings(
    autoCompilerPlugins := true,
    addCompilerPlugin("org.scala-lang.plugins" % "scala-continuations-plugin_2.11.7" % "1.0.2"),
    scalacOptions += "-P:continuations:enable",
    assemblyJarName in assembly := "experiments_cps.jar",
    mainClass in assembly := Some("scala.util.continuations.MicroBench")
  )

lazy val plugin = project
