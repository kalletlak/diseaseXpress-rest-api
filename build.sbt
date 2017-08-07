import com.typesafe.sbt.packager.MappingsHelper._
scalaVersion		:= "2.11.8"
sbtVersion			:= "0.13.13"
lazy val playVersion		= "2.4.0"
lazy val jacksonVersion		= "2.8.4"
lazy val jongoVersion		= "1.3.0"
lazy val enumeratumVersion	= "1.5.1"
lazy val swaggerVersion		= "1.5.2"

libraryDependencies ++= Seq(

  // play
  "com.typesafe.play" %% "play" % playVersion withSources () withJavadoc (),
  "io.swagger" %% "swagger-play2" % swaggerVersion withSources () withJavadoc (),
  "com.typesafe.play" %% "play-json" % "2.5.10" withSources () withJavadoc (),
  "org.jongo" % "jongo" % jongoVersion withSources () withJavadoc (),
  "org.mongodb" % "mongo-java-driver" % "3.4.2",

  "com.beachape" %% "enumeratum" % enumeratumVersion withSources () withJavadoc (),
  "com.beachape" %% "enumeratum-play-json" % enumeratumVersion withSources () withJavadoc (),
  "com.fasterxml.jackson.core" % "jackson-databind" % jacksonVersion,
  "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.8.3",
  "net.databinder.dispatch" %% "dispatch-core"   % "0.13.1")
  
libraryDependencies += filters

mappings in Universal ++= directory(baseDirectory.value / "public")
unmanagedResourceDirectories in Compile += { baseDirectory.value / "public" }

//https://stackoverflow.com/questions/25144484/sbt-assembly-deduplication-found-error
//https://github.com/swagger-api/swagger-samples/blob/master/scala/scala-play2.4/build.sbt
assemblyMergeStrategy in assembly := {
  case PathList("javax", "servlet", xs @ _*)                    => MergeStrategy.first
  case PathList("org", "apache", "commons", "logging", xs @ _*) => MergeStrategy.first
  case PathList(ps @ _*) if ps.last endsWith ".html"            => MergeStrategy.first
  case "application.conf"                                       => MergeStrategy.concat
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case x =>
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}

// for eclipse to link with sources
// TODO: shouldn't have IDE-specific settings in build file
EclipseKeys.withSource := true // see http://stackoverflow.com/questions/10472840/how-to-attach-sources-to-sbt-managed-dependencies-in-scala-ide
EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Resource // see http://stackoverflow.com/questions/14060131/access-configuration-resources-in-scala-ide

// so can use "sbt run"
lazy val disease_express = (project in file("."))
  .enablePlugins(PlayScala)
  .settings(
    name := "disease-express_es_rest_api",
    version := "1.2")
