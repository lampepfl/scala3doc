package dotty.dokka

import org.jetbrains.dokka.model._

object KotlinSupportTest extends App:
   KotlinSupport.create[DPackage] { val ala = 123 }