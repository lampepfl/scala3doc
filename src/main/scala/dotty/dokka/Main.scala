package dotty.dokka

import org.jetbrains.dokka._
import org.jetbrains.dokka.utilities._
import org.jetbrains.dokka.plugability._
import java.util.ServiceLoader
import java.io.File
import java.util.jar._
import collection.JavaConverters._

import scala.tasty.Reflection
import scala.tasty.inspector.TastyInspector
import dotty.tastydoc.representations
import dotty.tastydoc.representations._
import sbt.io.IO
import java.nio.file.Files

import org.kohsuke.args4j.{CmdLineParser, Option => COption}

class RawArgs:
    @COption(name="--tastyRoots", required = true, aliases = Array("-t"), usage="Roots where tools should look for tasty files")
    private var tastyRoots: String = null

    @COption(name="--output",required = true, aliases = Array("-o"), usage="Output to generate documentation to")
    private var output: String = "output"

    @COption(name="--classpath", aliases = Array("--cp", "-c"), usage="Classpath to load depenecies from")
    private var classpath: String = System.getProperty("java.class.path")

    @COption(name="--name", aliases = Array("-n"), usage="Name of documented project")
    private var name: String = "main"

    @COption(name="--docs", aliases = Array("-d"), usage="Root of project docs")
    private var docsRoot: String =  null
    
    @COption(name="--projectTitle")
    private var projectTitle: String = null

    @COption(name="--projectVersion")
    private var projectVersion: String = null

    @COption(name="--projectLogo")
    private var projectLogo: String = null

    def toArgs = Args(
      name,
      tastyRoots.split(File.pathSeparatorChar).toList.map(new File(_)),
      classpath,
      new File(output),
      Option(docsRoot),
      projectVersion,
      Option(projectTitle),
      Option(projectLogo)
    )

case class Args(
  name: String,
  tastyRoots: Seq[File],
  classpath: String,
  output: File,
  docsRoot: Option[String],
  projectVersion: String,
  projectTitle: Option[String],
  projectLogo: Option[String]
)    

case class DocConfiguration(tastyFiles: List[String], args: Args)
    
object Main:
  def main(args: Array[String]): Unit = 
    try 
      val rawArgs = new RawArgs
      new CmdLineParser(rawArgs).parseArgument(args:_*)
      val parsedArgs = rawArgs.toArgs

      val (jars, dirs) = parsedArgs.tastyRoots.partition(_.isFile)
      val extracted = jars.filter(_.exists()).map { jarFile =>
          val tempFile = Files.createTempDirectory("jar-unzipped").toFile
          IO.unzip(jarFile, tempFile)
          tempFile
      }

      try 
        def listTastyFiles(f: File): Seq[String] =
          val (files, dirs) = f.listFiles().partition(_.isFile)
          files.filter(_.getName.endsWith(".tasty")).map(_.toString) ++ dirs.flatMap(listTastyFiles)
        
        val tastyFiles = (dirs ++ extracted).flatMap(listTastyFiles).toList

        val config = DocConfiguration(tastyFiles, parsedArgs)

        if (parsedArgs.output.exists()) IO.delete(parsedArgs.output)

        // TODO #20 pass options, classpath etc.
        new DokkaGenerator(new DottyDokkaConfig(config), DokkaConsoleLogger.INSTANCE).generate()

        println("Done")
        
      
      finally
        extracted.foreach(IO.delete)
      // Sometimes jvm is hanging, so we want to be sure that we force shout down the jvm
      sys.exit(0)
    catch 
      case a: Exception =>
        a.printStackTrace()
        // Sometimes jvm is hanging, so we want to be sure that we force shout down the jvm
        sys.exit(1)     
