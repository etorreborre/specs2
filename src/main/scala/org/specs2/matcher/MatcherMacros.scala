package org.specs2
package matcher

import scala.reflect.macros.Context
import scala.annotation.StaticAnnotation

/**
 * Macro definitions to generate matchers for the members of a type T
 */
trait MatcherMacros {
  def matchA[T]  = macro org.specs2.matcher.MatcherMacros.matcherMacroImpl[T]
  def matchAn[T] = macro org.specs2.matcher.MatcherMacros.matcherMacroImpl[T]

  class fieldMatcherBody(tree: Any) extends StaticAnnotation
}

/**
 * Macros implementations
 *
 * The bulk of the implementation is credited to:
 *
 * - @travisbrown for his ["vampire methods"](http://meta.plasm.us/posts/2013/08/31/feeding-our-vampires/)
 * - @retronym for his help on [range positions](https://groups.google.com/forum/#!topic/scala-internals/fmWKw5TDz98)
 * - @xeno_by for his help on macros in general!
 */
object MatcherMacros extends MatcherMacros {

  def matcherMacroImpl[T : c.WeakTypeTag](c: Context): c.Expr[Any] = { import c.universe._
    val typeOfT = weakTypeOf[T]
    val matcherClassType = newTypeName(matcherClassName[T](c))

    val fields = typeOfT.members.filter(_.isPublic).
      filterNot(isConstructor(c)).
      filterNot(isSynthetic(c)).
      filter(_.owner != typeOf[Object].typeSymbol).
      filter(_.owner != typeOf[Product].typeSymbol).
      filter(_.owner != typeOf[Equals].typeSymbol)

    val (fieldValueMatchers, fieldMatchers, fieldFunctionMatchers) = fields.map { member =>
      val fieldName  = member.name.toString
      val methodName = newTermName(fieldName)
      val fieldType  = member.typeSignature
      val parameterName = newTermName(fieldName)

      val valueBody    = q"""(fieldValue: $fieldType) => addMatcher((t :$typeOfT) => new org.specs2.matcher.BeTypedEqualTo[$fieldType](fieldValue)(theValue[$fieldType](t.$parameterName)).toResult)"""
      val matcherBody  = q"""(matcherValue: org.specs2.matcher.Matcher[$fieldType]) => addMatcher((t :$typeOfT) => matcherValue[$fieldType](theValue[$fieldType](t.$parameterName)).toResult) """
      val functionBody = q"""(f: $fieldType => org.specs2.execute.Result) => addMatcher((t :$typeOfT) => f(t.$parameterName)) """

      (q""" @fieldMatcherBody($valueBody) def $methodName(fieldValue: $fieldType): $matcherClassType = macro org.specs2.matcher.MatcherMacros.fieldMatcherImplementation[$fieldType, $typeOfT] """,
       q""" @fieldMatcherBody($matcherBody) def $methodName(fieldValue: org.specs2.matcher.Matcher[$fieldType]): $matcherClassType = macro org.specs2.matcher.MatcherMacros.fieldMatcherImplementation[$fieldType, $typeOfT] """,
       q""" @fieldMatcherBody($functionBody) def $methodName[R : org.specs2.execute.AsResult](fieldValue: $fieldType => R): $matcherClassType = macro org.specs2.matcher.MatcherMacros.fieldMatcherImplementation2[$fieldType, $typeOfT, R] """)
    }.unzip3

    val matcherDefinition = q"""
      case class $matcherClassType(matcherOfT: Option[$typeOfT => org.specs2.execute.Result] = None) extends org.specs2.matcher.Matcher[$typeOfT] {
        // this is a workaround for 'missing type' if a default function value is specified in the case class
        private def matcherFunction = matcherOfT.getOrElse((t: $typeOfT) => org.specs2.execute.Success())

        def addMatcher(f: $typeOfT => org.specs2.execute.Result): $matcherClassType = copy(Some((t: $typeOfT) => matcherFunction(t) and f(t)))

        def apply[S <: $typeOfT](s: org.specs2.matcher.Expectable[S]) = {
          val r = matcherFunction(s.value)
          result(r.isSuccess, r.message, r.message, s)
        }

        ..$fieldMatchers
        ..$fieldValueMatchers
        ..$fieldFunctionMatchers
      }
    """

    val block = q"""
      $matcherDefinition
      new $matcherClassType {}
    """

    c.Expr(setMacroPosition(c).transform(block))
  }

  def fieldMatcherImplementation[F, T : c.WeakTypeTag](c: Context)(fieldValue: c.Expr[F]) = {
    import c.universe._

    val (arg, body) = extractBody(c)
    val prefixVal = newTermName(c.fresh)

    c.Expr(setMacroPosition(c).transform {
      q"""
        val $prefixVal = ${c.prefix.tree}
        val $arg = ${fieldValue.tree.duplicate}
        ${replaceThises(c)(matcherClassName[T](c), prefixVal).transform(body)}
      """})
  }

  def fieldMatcherImplementation2[F : c.WeakTypeTag, T : c.WeakTypeTag, R](c: Context)(fieldValue: c.Expr[F])(asResult: c.Expr[R]) = { import c.universe._

    val (arg, body) = extractBody(c)
    val prefixVal = newTermName(c.fresh)

    c.Expr(setMacroPosition(c).transform {
      q"""
        implicit class ToStrictAndThen[A, B](f: Function1[A, B]) { def andThenStrict[C](g: (=>B) => C): Function1[A, C] = (a: A) => g(f(a)) }

        val $prefixVal = ${c.prefix.tree}
        val $arg = ${fieldValue.tree.duplicate} andThenStrict ${asResult.tree}.asResult
        ${replaceThises(c)(matcherClassName[T](c), prefixVal).transform(body)}
      """})(implicitly[c.WeakTypeTag[F]])
  }

  private def matcherClassName[T : c.WeakTypeTag](c: Context) = c.universe.weakTypeOf[T].typeSymbol.name.encoded+"Matcher"

  private def isConstructor(c: Context) = { import c.universe._
    (s: Symbol) =>  s match {
      case m: c.universe.MethodSymbol => m.isConstructor
      case other                      => false
    }
  }

  private def isSynthetic(c: Context) = { import c.universe._
    (s: Symbol) => Seq("copy", "asInstanceOf", "isInstanceOf", "==", "!=").contains(s.name.decoded.toString)
  }

  private def extractBody(c: Context) = { import c.universe._
    c.macroApplication.symbol.annotations.find(_.tpe.toString.endsWith("fieldMatcherBody")).
      flatMap(_.scalaArgs.collectFirst { case Function(ValDef(_, a, _, _) :: Nil, b) => a -> c.resetAllAttrs(b) }).
      getOrElse(c.abort(c.enclosingPosition, "Annotation body not provided!"))
  }

  private def replaceThises(c: Context)(className: String, prefixVal: c.TermName) = {
    import c.universe._
    new Transformer {
      override def transform(tree: Tree) = tree match {
        case This(qualifier) if qualifier.decoded.startsWith(className) => Ident(prefixVal)
        case other                                                      => super.transform(other)
      }
    }
  }

  /** set a specific position to the elements of a tree  */
  private def setPosition(c: Context)(position: c.Position) = { import c.universe._
    new Transformer {
      override def transform(tree: Tree) = tree match {
        case t => super.transform(t).setPos(position)
      }
    }
  }
  /** set the macro application position to the elements of a tree  */
  private def setMacroPosition(c: Context) = setPosition(c)(c.macroApplication.pos.makeTransparent)

}

