package org.specs2
package specification

import form.Form
import main.{Arguments, ArgumentsArgs}
import execute._
import text.NotNullStrings._
import text.Trim._
import control.Exceptions._
import scala.reflect.macros.{Context => MContext}
import text.{CodeMarkup, NoMarkup}

/**
 * Allow to use fragments inside interpolated strings starting with s2 in order to build the specification content
 */
trait SpecificationStringContext { outer: FragmentsBuilder with ArgumentsArgs =>

  implicit def exampleIsSpecPart(e: Example): SpecPart = new SpecPart {
    def appendTo(text: String, expression: String = "") = text ^ e
  }

  implicit def asResultIsSpecPart[R : AsResult](r: =>R): SpecPart = new SpecPart {
    def appendTo(text: String, expression: String = "") = {
      val texts = text.split("\n")
      val first = texts.dropRight(1).mkString("", "\n", "\n")
      val autoExample = texts.last.trim.isEmpty
      val indent = texts.last.takeWhile(Seq(' ', '\n').contains).mkString

      val description = if (autoExample) CodeMarkup(indent+expression) else NoMarkup(texts.last)
      val before = if (autoExample) first else (first + indent)

      implicitly[AsResult[R]] match {
        case v : AnyValueAsResult[_] => AsResult(r) match {
          case DecoratedResult(t, e: Error) => before ^ exampleFactory.newExample(description, e)
          case DecoratedResult(t, _)        => textStart(text + t.notNull)
        }
        case other                        => before ^ exampleFactory.newExample(description, AsResult(r))
      }
    }
  }
  implicit def anyAsResultIsSpecPart(r: =>Function0Result): SpecPart = new SpecPart {
    def appendTo(text: String, expression: String = "") = asResultIsSpecPart(AsResult(r)).appendTo(text, expression)
  }
  implicit def formIsSpecPart(f: =>Form): SpecPart = new SpecPart {
    def appendTo(text: String, expression: String = "") = text ^ Fragments.createList(Forms.formsAreExamples(f.executeForm))
  }
  implicit def toFormIsSpecPart(f: { def form: Form}): SpecPart = new SpecPart {
    def appendTo(text: String, expression: String = "") = formIsSpecPart(f.form).appendTo(text, expression)
  }
  implicit def fragmentIsSpecPart(f: Fragment): SpecPart = new SpecPart {
    def appendTo(text: String, expression: String = "") = text ^ f
  }
  implicit def fragmentsIsSpecPart(fs: Fragments): SpecPart = new SpecPart {
    def appendTo(text: String, expression: String = "") = text ^ fs
  }
  implicit def argumentsIsSpecPart(a: Arguments): SpecPart = new SpecPart {
    def appendTo(text: String, expression: String = "") = text ^ a
  }

  implicit class specificationInStringContext(sc: StringContext) {
    def s2(variables: SpecPart*) = macro S2Macro.s2Implementation
  }

  def s2(fileContent: String, texts: Seq[String], variables: Seq[SpecPart]) = {
    def textOffsets(ts: Seq[String]) = {
      ts.foldLeft(((fileContent, ""), Seq[Int]())) { (res, cur) =>
        val ((remaining, passed), result) = res
        val i = remaining.indexOf(cur)
        ((remaining.substring(i + cur.size), passed + remaining.substring(0, i + cur.size)), result :+ (passed.size + i))
      }._2
    }
    // start offsets for text elements in the full content
    def textStartOffsets = textOffsets(texts)
    // end offsets for text elements in the full content
    def textEndOffsets = (textStartOffsets zip texts).map { case (start, t) => start + t.size - 1 }

    val trimExpression = (e: String) => e.removeFirst("\\Q${\\E").removeFirst("\\Q$\\E").removeLast("\\Q}\\E")
    val expressions = (textEndOffsets.dropRight(1) zip textStartOffsets.drop(1)).map { case (start, end) =>
      tryo(fileContent.substring(start, end)).getOrElse("failed to retrieve text between: "+(start, end))
    }.map(trimExpression)

    (texts zip variables zip expressions).foldLeft(Fragments.createList() ^ args.report(noindent = true) ^ args.report(flow = true)) { (res, cur) =>
      val ((text, variable), expression) = cur
      val appended = variable.appendTo(text, expression)
    
      // try to keep contiguous text fragments as one so that they can be properly rendered as Markdown
      // like numbered lists for example
      (res.middle, appended.middle) match {
        case (begin :+ Text(t1), Text(t2) +: rest) => (res ^ appended).copy(middle = begin ++ (Text(t1+t2) +: rest))
        case _                                     => res ^ appended
      }
    } ^ texts.lastOption.getOrElse("")
  }

}

object S2Macro {
  def s2Implementation(c : MContext)(variables: c.Expr[SpecPart]*) : c.Expr[Fragments] = {
    import c.{universe => u}; import u.{ Position => _, _ }
    def toAST[A : TypeTag](xs: Tree*): Tree =
      Apply(Select(Ident(typeOf[A].typeSymbol.companionSymbol), newTermName("apply")), xs.toList)

    def methodCall(name: String, xs: Tree*): Tree =
      Apply(Select(This(tpnme.EMPTY), newTermName(name)), xs.toList)

    val content = c.macroApplication.pos.source.content.mkString
    val texts = c.prefix.tree match { case Apply(_, List(Apply(_, ts))) => ts }
    val vs = variables.map(_.tree)
    c.Expr(methodCall("s2", c.literal(content).tree, toAST[List[_]](texts:_*), toAST[List[_]](vs:_*)))
  }

}

trait SpecPart {
  def appendTo(text: String, expression: String = ""): Fragments
}

