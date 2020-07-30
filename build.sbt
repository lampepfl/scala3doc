val dottyVersion = "0.25.0"
val dokkaVersion = "1.4.0-M3-dev-81"

libraryDependencies += "org.jetbrains.dokka" % "dokka-base" % dokkaVersion
libraryDependencies += "org.jetbrains.dokka" % "dokka-core" % dokkaVersion
libraryDependencies += "org.jetbrains.dokka" % "dokka-test-api" % dokkaVersion
libraryDependencies += "ch.epfl.lamp" %% "dotty-tastydoc" % dottyVersion
libraryDependencies += "ch.epfl.lamp" %% "dotty-compiler" % dottyVersion
libraryDependencies += "ch.epfl.lamp" %% "dotty-library" % dottyVersion

resolvers += Resolver.jcenterRepo

resolvers += Resolver.bintrayRepo("kotlin", "kotlin-dev")

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-dokka",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  )


 val dokkaJavaApiJar = file("libs") / "dokkaJavaApi-0.1.0.jar"


val buildDokkaApi = taskKey[File]("Compile dokka wrapper and put jar in lib")
buildDokkaApi := {
  val gradleRootDir = file("dokkaJavaApi")
  sys.process.Process(Seq("./gradlew", "build"), gradleRootDir).!

  if (dokkaJavaApiJar.exists()) IO.delete(dokkaJavaApiJar)
  IO.move(gradleRootDir / "build" / "libs" / "dokkaJavaApi-0.1.0.jar", dokkaJavaApiJar)
  streams.value.log.success(s"Dokka api copied to $dokkaJavaApiJar")
  dokkaJavaApiJar
}

val generateSelfDocumentation = inputKey[Unit]("Generate example documentation")
generateSelfDocumentation := {
  file("output").delete()   
  run.in(Compile).fullInput(" target/scala-0.25/classes").evaluated // TODO 
}

unmanagedJars in Compile += dokkaJavaApiJar

// Uncomment to debug dokka processing (require to run debug in listen mode on 5005 port)
//javaOptions.in(run) += "-agentlib:jdwp=transport=dt_socket,server=n,address=localhost:5005,suspend=y"

fork.in(run) := true
// There is a bug in dokka that prevents parallel tests withing the same jvm
fork.in(test) := true
Test / parallelExecution := false

scalacOptions in Compile += "-language:implicitConversions"
