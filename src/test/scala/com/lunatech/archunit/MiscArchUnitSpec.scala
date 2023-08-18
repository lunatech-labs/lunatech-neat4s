package com.lunatech.archunit

import com.lunatech.archunit.BaseArchUnitSpec.importedClasses
import com.tngtech.archunit.core.domain.JavaCall.Predicates.target
import com.tngtech.archunit.core.domain.JavaClass.Predicates.equivalentTo
import com.tngtech.archunit.core.domain.properties.HasName.Predicates.nameMatching
import com.tngtech.archunit.core.domain.properties.HasOwner.Predicates.With.owner
import com.tngtech.archunit.lang.syntax.ArchRuleDefinition.noClasses
import org.scalatest.wordspec.AnyWordSpecLike

class MiscArchUnitSpec extends AnyWordSpecLike with BaseArchUnitSpec {

  "Classes" should {

    "never call println(...)" in {
      noClasses.should
        .callMethodWhere {
          target {
            owner {
              equivalentTo(scala.Predef.getClass)
            }
          } and target {
            nameMatching("println")
          }
        }
        .check(importedClasses)
    }
  }
}
