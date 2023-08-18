package com.lunatech.archunit

import java.lang.annotation.Annotation
import java.util.regex.Pattern

import scala.reflect.ClassTag

import com.tngtech.archunit.core.domain.JavaClass
import com.tngtech.archunit.core.domain.JavaClasses
import com.tngtech.archunit.core.domain.JavaModifier
import com.tngtech.archunit.core.importer.ClassFileImporter
import com.tngtech.archunit.core.importer.Location

trait BaseArchUnitSpec {
  implicit class JavaClassOps(self: JavaClass) {
    def isConcrete: Boolean = {
      !(self.isInterface || self.getModifiers.contains(JavaModifier.ABSTRACT))
    }

    def hasAnnotation[T <: Annotation](implicit classTag: ClassTag[T]): Boolean = {
      self.isAnnotatedWith(classTag.runtimeClass.asInstanceOf[Class[_ <: Annotation]])
    }
  }
}

object BaseArchUnitSpec {
  // Let's import classes just once in the object lifetime (and not once per test suite instance) to speed things up.
  private val classFileImporter = new ClassFileImporter()
  lazy val importedClasses: JavaClasses = classFileImporter.importPackages("com.lunatech.neat4s")
  lazy val importedProductionClasses: JavaClasses = classFileImporter
    .withImportOption((location: Location) => {
      !location.matches(Pattern.compile(".*/target/scala-.*/test-classes/.*"))
    })
    .importPackages("com.lunatech.neat4s")
}
