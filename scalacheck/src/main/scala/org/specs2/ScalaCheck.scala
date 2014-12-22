package org.specs2

import org.specs2.scalacheck._
import specification._
import create._


trait ScalaCheck extends
  ScalaCheckPropertyCreation with
  ScalaCheckPropertyCheck with
  ScalaCheckParameters with
  AsResultProp with
  ScalaCheckPropertyDsl with
  GenInstances
