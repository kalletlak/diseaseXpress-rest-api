import com.typesafe.sbt.packager.MappingsHelper._
scalaVersion		:= "2.11.8"
sbtVersion			:= "0.13.13"
lazy val playVersion		= "2.4.0"
lazy val jacksonVersion		= "2.8.4"
lazy val jongoVersion		= "1.3.0"
lazy val enumeratumVersion	= "1.5.1"
lazy val swaggerVersion		= "1.5.2"
lazy val log4jVersion = "1.2.17"
lazy val elastic4sVersion = "5.4.12"

libraryDependencies ++= Seq(

  // play
  "com.typesafe.play" %% "play" % playVersion withSources () withJavadoc (),
  "io.swagger" %% "swagger-play2" % swaggerVersion withSources () withJavadoc (),
  "com.typesafe.play" %% "play-json" % "2.5.9" withSources () withJavadoc (),
  
  
  "org.jongo" % "jongo" % jongoVersion withSources () withJavadoc (),
  "org.mongodb" % "mongo-java-driver" % "3.4.2",
  // https://mvnrepository.com/artifact/com.github.fakemongo/fongo
  "com.github.fakemongo" % "fongo" % "2.1.0" withSources() withJavadoc() ,

  //elasticsearch client
    "com.sksamuel.elastic4s" %% "elastic4s-core" % elastic4sVersion,
  // for the tcp client
  //"com.sksamuel.elastic4s" %% "elastic4s-tcp" % elastic4sVersion,
  
  // for the http client
  "com.sksamuel.elastic4s" %% "elastic4s-http" % elastic4sVersion,
  
  "com.beachape" %% "enumeratum" % enumeratumVersion withSources () withJavadoc (),
  "com.beachape" %% "enumeratum-play-json" % enumeratumVersion withSources () withJavadoc (),
  "com.fasterxml.jackson.core" % "jackson-databind" % jacksonVersion,
  "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.8.3",
  "net.databinder.dispatch" %% "dispatch-core"   % "0.13.1",
  "com.datastax.cassandra" % "cassandra-driver-core" % "3.3.0",
  "com.datastax.cassandra" % "cassandra-driver-mapping" % "3.3.0",
  "com.datastax.cassandra" % "cassandra-driver-extras" % "3.3.0",
  
  "org.slf4j" % "slf4j-simple" % "1.7.21",
  "org.apache.logging.log4j" % "log4j-api" % "2.6.2",
  "org.apache.logging.log4j" % "log4j-to-slf4j" % "2.6.2"
  

  
  )
  
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
  case PathList("org", "slf4j", xs @ _*) => MergeStrategy.last
  case  x if x.endsWith("com/mongodb/util/JSONParser.class")  => MergeStrategy.first
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
