import sbt._

object Dependency {

  val resolutionRepos = Seq(
    "spray repo" at "http://repo.spray.io/",
    "Sonatype OSS Releases Repository" at "http://oss.sonatype.org/content/repositories/releases"
  )


  object V { 
    val Akka         = "2.2.0-RC1"   // Apache 2.0
    val Caliper      = "0.5-rc1"     // Apache 2.0
    val HtmlParser   = "1.4"         // MIT
    val Logback      = "1.0.9"       // EPL 1.0/LGPL 2.1
    val Parboiled    = "1.1.4"       // Apache 2.0
    val ScalaTest    = "2.0.M5b"     // Apache 2.0
    val Spray        = "1.2-M8"      // Apache 2.0
    val Webjars      = "0.4"         // Apache 2.0
  }

  object Compile {
    val akkaActor       = "com.typesafe.akka"           %%  "akka-actor"            % V.Akka
    val akkaSlf4j       = "com.typesafe.akka"           %%  "akka-slf4j"            % V.Akka
    val caliper         = "com.google.caliper"          %   "caliper"               % V.Caliper
    val htmlParser      = "nu.validator.htmlparser"     %   "htmlparser"            % V.HtmlParser
    val logbackClassic  = "ch.qos.logback"              %   "logback-classic"       % V.Logback
    val parboiled       = "org.parboiled"               %%  "parboiled-scala"       % V.Parboiled
    val sprayCaching    = "io.spray"                    %   "spray-caching"         % V.Spray
    val sprayCan        = "io.spray"                    %   "spray-can"             % V.Spray
    val sprayRouting    = "io.spray"                    %   "spray-routing"         % V.Spray
    val webjarsLocator  = "org.webjars"                 %   "webjars-locator"       % "0.4"
  }

  object Test {
    val akkaTestkit     = "com.typesafe.akka"           %%  "akka-testkit"          % V.Akka           % "test"
    val scalaTest       = "org.scalatest"               %%  "scalatest"             % V.ScalaTest      % "test"
    val sprayTestkit    = "io.spray"                    %   "spray-testkit"         % V.Spray          % "test"
  }

  object Runtime {
    val bootstrap       = "org.webjars"                 % "bootstrap"               % "2.1.1"          % "runtime"
    val jQuery          = "org.webjars"                 % "jquery"                  % "2.0.2"          % "runtime"
  }
}
