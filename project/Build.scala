import sbt._
import sbt.Keys._
import spray.revolver.RevolverPlugin._


object Build extends Build {
  import Settings._


  lazy val islay = Project(
    id = "islay",
    base = file("."),
    settings = rootSettings,
    aggregate = Seq(transform, template, web)
  ).settings( Defaults.itSettings : _*)

  lazy val transform = Project(
    id = "transform",
    base = file("transform"),
    settings = transformSettings
  ).settings( libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _) )

  lazy val template = Project(
    id = "template",
    base = file("template"),
    settings = templateSettings,
    dependencies = Seq(transform)
  )

  lazy val web = Project(
    id = "web",
    base = file("web"),
    settings = webSettings,
    dependencies = Seq(transform, template)
  )

  lazy val benchmark = Project(
    id = "benchmark",
    base = file("benchmark"),
    settings = benchmarkSettings,
    dependencies = Seq(web)
  )

  lazy val example = Project(
    id = "example",
    base = file("example"),
    settings = exampleSettings,
    dependencies = Seq(web)
  )

  override lazy val settings = super.settings :+ {
    shellPrompt := { s => Project.extract(s).currentProject.id + " > " }
  }
}


object Settings {
  import Dependencies._


  lazy val buildSettings = Seq(
    organization    := "io.islay",
    version         := "0.1",
    startYear       := Some(2013),
    licenses        := Seq("Apache 2" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt"))
  )


  lazy val defaultSettings = Defaults.defaultSettings ++ buildSettings ++ Seq(
    resolvers       ++= Dependency.resolutionRepos,
    scalaVersion    := "2.10.1",
    scalacOptions   ++= Seq("-encoding", "UTF-8", "-deprecation", "-unchecked", "-feature")
  )

  lazy val rootSettings = defaultSettings ++ Seq(
    publishArtifact in Compile := false
  )

  lazy val transformSettings = defaultSettings ++ Seq(
    libraryDependencies ++=
      Dependencies.transform ++
      Dependencies.transformTestkit,
    scalacOptions in (Compile, console) += "-Yreify-copypaste"
  )

  lazy val templateSettings = defaultSettings ++ Seq(
    libraryDependencies ++=
      Dependencies.template ++
      Dependencies.templateTestkit
  )

  lazy val webSettings = defaultSettings ++ Seq(
    libraryDependencies ++=
      Dependencies.web ++
      Dependencies.webTestkit
  )

  lazy val benchmarkSettings = defaultSettings ++ Seq(
    libraryDependencies ++=
      Dependencies.benchmark,
    fork in run := true,
    // we need to add the runtime classpath as a "-cp" argument to the `javaOptions in run`, otherwise caliper
    // will not see the right classpath and die with a ConfigurationException
    javaOptions in run <++= (fullClasspath in Runtime) map { cp => Seq("-cp", sbt.Build.data(cp).mkString(":")) }
  )

  lazy val exampleSettings = defaultSettings ++ Revolver.settings ++ Seq(
    libraryDependencies ++=
      Dependencies.example ++
      Dependencies.exampleTestkit,
      watchSources ~= { ws => ws filter { f => f.getName.endsWith(".conf") || !f.getAbsolutePath.contains("src/main/resources") } }
  )
}


object Dependencies {
  import Dependency._

  val baseTestkit = Seq(Test.scalaTest)

  val transform = Seq(Compile.parboiled)
  val transformTestkit = baseTestkit

  val template = Seq(
    Compile.akkaActor,
    Compile.htmlParser,
    Compile.sprayCaching,
    Compile.sprayRouting
  )
  val templateTestkit = baseTestkit ++ Seq(Test.akkaTestkit)

  val web = Seq(
    Compile.akkaActor,
    Compile.logbackClassic,
    Compile.sprayCaching,
    Compile.sprayRouting,
    Compile.webjarsLocator
  )
  val webTestkit = baseTestkit ++ Seq(Test.sprayTestkit)

  val benchmark = Seq(Compile.caliper)

  val example = Seq(
    Compile.sprayCan,
    Runtime.bootstrap,
    Runtime.jQuery
  )
  val exampleTestkit = baseTestkit ++ Seq(Test.sprayTestkit)
}
