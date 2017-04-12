package org.specs2.runner

import sbt.testing.SubclassFingerprint

trait SpecificationFingerprint extends SubclassFingerprint {
  def isModule = true

  def superclassName = "org.specs2.specification.core.SpecificationStructure"

  def requireNoArgConstructor = false
}

object Fingerprints {
  val fp1 = new SpecificationFingerprint {
    override def toString = "specs2 Specification fingerprint"
  }
  val fp1m = new SpecificationFingerprint {
    override def toString = "specs2 Specification fingerprint"

    override def isModule = false
  }
}

