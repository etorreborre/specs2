package org.specs2
package reporter
import specification._
import main._
import ExecutedSpecificationData._

class HtmlExportingSpec extends Specification with ScalaCheck { def is =
                                                                                                                                            """
The HtmlExporting trait is responsible for exporting the executed specification:

  ExecutedSpecification => Unit

While the formal type of output is Unit, the HtmlExporting trait actually transforms the ExecutedSpecification to a sequence of HtmlFiles to write to disk:

  `ExecutedSpecification => Seq[HtmlFile] => Unit`

  where `HtmlFile  = (Url, NodeSeq)`
                                                                                                                                            """^
  "The number of created HtmlFile must be 1 + number of linked specifications"                                                              ! e1^
                                                                                                                                            end
  def exporter = new HtmlExporting {}

  def e1 = check { (spec: ExecutedSpecification) =>
    (1 + spec.includedLinkedSpecifications.size) === exporter.print(spec)(Arguments()).size
  }
}

//spec[ExecutedSpecification, Arguments, Unit](new HtmlExporting0Spec)
//
//def spec[A, B](spec: FunctionSpecification[A, B]): FunctionSpecification[A, B] = spec
//def spec[A, B, C](spec: Function2Specification[A, B, C]): Function2Specification[A, B, C] = spec
//
//
//trait HtmlExporting1 {
//  import internal.scalaz.Scalaz._
//  def fragmentsToLines: ExecutedSpecification  => Seq[HtmlLinesFile]
//  def linesToHtml: Seq[HtmlLinesFile] => Seq[HtmlFile]
//  def writeFiles: Seq[HtmlFile] => Unit
//
//  def export: (ExecutedSpecification, Arguments) => Unit = { case (spec, args) =>
//    spec |> fragmentsToLines |> linesToHtml |> writeFiles
//  }
//}


trait FunctionSpecification[A, B] extends Specification with Function[A, B] {
  def data: DataSpecificationStructure[A]
  def function: A => B
  def apply(a: A) = function(a)
}
trait Function2Specification[A, B, C] extends Specification with Function2[A, B, C] {
  def data: (DataSpecificationStructure[A], DataSpecificationStructure[B])
  def function: (A, B) => C
  def apply(a: A, b: B) = function(a, b)
}


class HtmlExporting0Spec extends Function2Specification[ExecutedSpecification, Arguments, Unit] {
  def data = (new ExecutedSpecificationSpec, new ArgumentsSpec)
  def function = { case (spec, args) => new HtmlExporting {}.export(args)(spec) }
  def is = success
}


class ExecutedSpecificationSpec extends DataSpecification[ExecutedSpecification] {
  def is = success
} 


class ArgumentsSpec extends DataSpecification[Arguments] {
  def is = success
}