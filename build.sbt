scalaVersion := "2.11.8"
sbtVersion := "0.13.13"
lazy val playVersion		= "2.5.13"
lazy val jacksonVersion		= "2.8.4"

libraryDependencies ++= Seq(

  // play
  "com.typesafe.play" %% "play"      % playVersion withSources() withJavadoc(),
  "com.typesafe.play" %% "play-docs" % playVersion withSources() withJavadoc(),

	  
	  // http://stackoverflow.com/questions/28270621/using-jackson-to-de-serialize-a-scala-case-class
      "com.fasterxml.jackson.core"   %  "jackson-databind" % jacksonVersion,
	  
	  "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.8.3",
	  
	  // https://mvnrepository.com/artifact/com.sksamuel.elastic4s/elastic4s-core_2.11
	  "com.sksamuel.elastic4s" % "elastic4s-core_2.11" % "2.4.0",
	  
	  // https://mvnrepository.com/artifact/com.sksamuel.elastic4s/elastic4s-play-json_2.11
	  "com.sksamuel.elastic4s" % "elastic4s-play-json_2.11" % "2.4.0",
	  // https://mvnrepository.com/artifact/com.sksamuel.elastic4s/elastic4s-jackson_2.11
	  "com.sksamuel.elastic4s" % "elastic4s-jackson_2.11" % "2.4.0"
	  
	  
 	  
//    "com.fasterxml.jackson.module" %% "jackson-module"   % jacksonVersion

)

// for eclipse to link with sources
// TODO: shouldn't have IDE-specific settings in build file
EclipseKeys.withSource := true // see http://stackoverflow.com/questions/10472840/how-to-attach-sources-to-sbt-managed-dependencies-in-scala-ide
EclipseKeys.createSrc  := EclipseCreateSrc.Default + EclipseCreateSrc.Resource // see http://stackoverflow.com/questions/14060131/access-configuration-resources-in-scala-ide

// so can use "sbt run"
lazy val disease_express = (project in file(".")).enablePlugins(PlayScala)

assemblyMergeStrategy in assembly := {
 case PathList("META-INF", xs @ _*) => MergeStrategy.discard
 case x => MergeStrategy.first
}