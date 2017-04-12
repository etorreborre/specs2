package org.specs2.runner

import org.specs2.runner.Fingerprints.{fp1, fp1m}
import sbt.testing.{Fingerprint, Framework}


/**
 * Implementation of the Framework interface for the sbt tool.
 * It declares the classes which can be executed by the specs2 library.
 */
class Specs2Framework extends Framework {
  def name = "specs2"

  def fingerprints = Array[Fingerprint](fp1, fp1m)

  def runner(args: Array[String], remoteArgs: Array[String], loader: ClassLoader) =
    new MasterSbtRunner(args, remoteArgs, loader)

  def slaveRunner(args: Array[String], remoteArgs: Array[String], loader: ClassLoader, send: String => Unit) =
    new SlaveSbtRunner(args, remoteArgs, loader, send)
}

