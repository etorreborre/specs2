package org.specs2
package reporter
import specification._
import main._

class HtmlExportingSpec extends Specification { def is =
                                                                                                                                            """
The HtmlExporting trait is responsible for exporting the executed specification:

  ExecutedSpecification => Unit

While the formal type of output is Unit, as for any export, it is expected that we have intermediate Html files written to disk:

  (specName, Seq[ExecutedFragment]) => Seq[HtmlLinesFile] => Seq[HtmlFile] => Unit

   
  HtmlFile  = (url, NodeSeq)
  HtmlLinesFi

                                                                                                                                            """

"The HtmlExporting trait is responsible for exporting the executed specification"
  spec[ExecutedSpecification, Arguments, Unit](new HtmlExporting0Spec)

  def spec[A, B](spec: FunctionSpecification[A, B]): FunctionSpecification[A, B] = spec
  def spec[A, B, C](spec: Function2Specification[A, B, C]): Function2Specification[A, B, C] = spec


  trait HtmlExporting1 {
    import internal.scalaz.Scalaz._
    def fragmentsToLines: ExecutedSpecification  => Seq[HtmlLinesFile]
    def linesToHtml: Seq[HtmlLinesFile] => Seq[HtmlFile]
    def writeFiles: Seq[HtmlFile] => Unit

    def export: (ExecutedSpecification, Arguments) => Unit = { case (spec, args) =>
      spec |> fragmentsToLines |> linesToHtml |> writeFiles
    }
  }


}

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

trait DataSpecificationStructure[+T] extends SpecificationStructure
trait DataSpecification[+T] extends DataSpecificationStructure[T] with Specification
class ExecutedSpecificationSpec extends DataSpecification[ExecutedSpecification] {
  def is = success
} 

class ArgumentsSpec extends DataSpecification[Arguments] {
  def is = success
}