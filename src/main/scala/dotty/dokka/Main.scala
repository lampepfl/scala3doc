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

case class DocConfiguration(tastyFiles: List[String], classpath: String)

object Main:
  def main(args: Array[String]): Unit =
    val Seq(outPath, tastyRoots) = args.take(2).toSeq
    val cp = args.drop(2).headOption.getOrElse(System.getProperty("java.class.path"))
    
    val (jars, dirs) = tastyRoots.split(File.pathSeparator).toList.map(File(_)).partition(_.isFile)
    val extracted = jars.filter(_.exists()).map { jarFile =>
        val tempFile = Files.createTempDirectory("jar-unzipped").toFile
        IO.unzip(jarFile, tempFile)
        tempFile
    }

    try 
      def listTastyFiles(f: File): Seq[String] =
        val (files, dirs) = f.listFiles().partition(_.isFile)
        files.filter(_.getName.endsWith(".tasty")).map(_.toString) ++ dirs.flatMap(listTastyFiles)
      
      val tastyFiles = (dirs ++ extracted).flatMap(listTastyFiles)

      val config = DocConfiguration(tastyFiles, cp)

      val out = new File(outPath)
      if (out.exists()) IO.delete(out)

      // TODO #20 pass options, classpath etc.
      new DokkaGenerator(new DottyDokkaConfig(config, out), DokkaConsoleLogger.INSTANCE).generate()
      println("Done")
    finally
      extracted.foreach(IO.delete)  
