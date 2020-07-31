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

case class DocConfiguration(tastyFiles: List[String], classpath: String)

object Main:
  def main(args: Array[String]): Unit =
    val tastyRoots = args.head
    val cp = args.drop(1).headOption.getOrElse(System.getProperty("java.class.path"))  
    def listTastyFiles(f: File): Seq[String] =
      val (files, dirs) = f.listFiles().partition(_.isFile)
      files.filter(_.getName.endsWith(".tasty")).map(_.toString) ++ dirs.flatMap(listTastyFiles)
    
    val tastyFiles = tastyRoots.split(File.pathSeparator).toList.map(File(_)).filter(_.exists).flatMap(listTastyFiles)

    val config = DocConfiguration(tastyFiles, cp)

    // TODO #20 pass options, classpath etc.
    new DokkaGenerator(new DottyDokkaConfig(config), DokkaConsoleLogger.INSTANCE).generate()
    println("Done")
