package org.specs2
package matcher

import reflect.MacroContext._
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
    val (matcherClassType, classDefinition) = new MakeMatchers[c.type](c).matchers[T]

    val block = q"""
      $classDefinition
      new $matcherClassType {}
    """

    c.Expr(setMacroPosition(c).transform(block))
  }

  def fieldMatcherImplementation[F, T : c.WeakTypeTag](c: Context)(fieldValue: c.Expr[F]) = {
    import c.universe._

    val (arg, body) = extractBody(c)
    val prefixVal = TermName(c.freshName)

    c.Expr(setMacroPosition(c).transform {
      q"""
        val $prefixVal = ${c.prefix.tree}
        val $arg = ${fieldValue.tree.duplicate}
        ${replaceThises(c)(matcherClassName[T](c), prefixVal).transform(body)}
      """})
  }

  def fieldMatcherImplementation2[F : c.WeakTypeTag, T : c.WeakTypeTag, R](c: Context)(fieldValue: c.Expr[F])(asResult: c.Expr[R]) = { import c.universe._

    val (arg, body) = extractBody(c)
    val prefixVal = TermName(c.freshName)

    c.Expr(setMacroPosition(c).transform {
      q"""
        implicit class ToStrictAndThen[A, B](f: Function1[A, B]) { def andThenStrict[C](g: (=>B) => C): Function1[A, C] = (a: A) => g(f(a)) }

        val $prefixVal = ${c.prefix.tree}
        val $arg = ${fieldValue.tree.duplicate} andThenStrict ${asResult.tree}.asResult
        ${replaceThises(c)(matcherClassName[T](c), prefixVal).transform(body)}
      """})(implicitly[c.WeakTypeTag[F]])
  }

  def matcherClassName[T : c.WeakTypeTag](c: Context) = c.universe.weakTypeOf[T].typeSymbol.name.encodedName.toString+"Matcher"

  private def extractBody(c: Context) = { import c.universe._
    c.macroApplication.symbol.annotations.find(_.tree.tpe.toString.endsWith("fieldMatcherBody")).
      flatMap(_.tree.children.tail.map(arg => c.untypecheck(arg)).collectFirst { case Function(ValDef(_, a, _, _) :: Nil, b) => a -> b }).
      getOrElse(c.abort(c.enclosingPosition, "Annotation body not provided!"))
  }

  private def replaceThises(c: Context)(className: String, prefixVal: c.TermName) = {
    import c.universe._
    new Transformer {
      override def transform(tree: Tree) = tree match {
        case This(qualifier) if qualifier.decodedName.toString.startsWith(className) => Ident(prefixVal)
        case other                                                                   => super.transform(other)
      }
    }
  }

  /** set a specific position to the elements of a tree  */
  private def setPosition(c: Context)(position: c.Position) = {
    import c.universe._

    new Transformer {
      override def transform(tree: Tree) = tree match {
        case t => internal.setPos(super.transform(t), position)
      }
    }
  }
  /** set the macro application position to the elements of a tree  */
  private def setMacroPosition(c: Context) = setPosition(c)(c.macroApplication.pos.makeTransparent)

}

class MakeMatchers[C <: Context](val c: C) {

  import c.universe._

  def matchers[T: c.WeakTypeTag] = {
    val typeOfT = weakTypeOf[T]
    val matcherClassType = TypeName(MatcherMacros.matcherClassName[T](c))

    val fields: Iterable[c.Symbol] = typeOfT.members.filter(_.isPublic).
      filter(isGetter(c)).
      filterNot(isSynthetic(c)).
      filter(_.owner != typeOf[Any].typeSymbol).
      filter(_.owner != typeOf[Object].typeSymbol).
      filter(_.owner != typeOf[Product].typeSymbol).
      filter(_.owner != typeOf[Equals].typeSymbol)

    val (fieldValueMatchers, fieldMatchers, fieldFunctionMatchers) = fields.map {
      member =>
        val fieldName = member.name.toString
        val methodName = TermName(fieldName)
        val fieldType = member.typeSignature
        val parameterName = TermName(fieldName)

        val valueBody = q"""(fieldValue: $fieldType) =>
        addMatcher((t :$typeOfT) => new org.specs2.matcher.BeTypedEqualTo[$fieldType](fieldValue)(theValue[$fieldType](t.$parameterName).updateDescription(d => "  "+$fieldName+": "+d)).toResult)"""
        val matcherBody = q"""(matcherValue: org.specs2.matcher.Matcher[$fieldType]) =>
        addMatcher((t :$typeOfT) => matcherValue[$fieldType](theValue[$fieldType](t.$parameterName).updateDescription(d => "  "+$fieldName+": "+d)).toResult) """
        val functionBody = q"""(f: $fieldType => org.specs2.execute.Result) => addMatcher((t :$typeOfT) => f(t.$parameterName)) """


        val x = (
          q""" @fieldMatcherBody($valueBody) def $methodName(fieldValue: $fieldType): $matcherClassType = macro org.specs2.matcher.MatcherMacros.fieldMatcherImplementation[$fieldType, $typeOfT] """,
          q""" @fieldMatcherBody($matcherBody) def $methodName(fieldValue: org.specs2.matcher.Matcher[$fieldType]): $matcherClassType = macro org.specs2.matcher.MatcherMacros.fieldMatcherImplementation[$fieldType, $typeOfT] """,
          q""" @fieldMatcherBody($functionBody) def $methodName[R : org.specs2.execute.AsResult](fieldValue: $fieldType => R): $matcherClassType = macro org.specs2.matcher.MatcherMacros.fieldMatcherImplementation2[$fieldType, $typeOfT, R] """
          )

        x: (Tree, Tree, Tree)
    }.unzip3

    val classDefinition = q"""
      case class $matcherClassType(matcherOfT: Option[$typeOfT => org.specs2.execute.Result] = None) extends org.specs2.matcher.Matcher[$typeOfT] {
        // this is a workaround for 'missing type' if a default function value is specified in the case class
        private def matcherFunction = matcherOfT.getOrElse((t: $typeOfT) => org.specs2.execute.Success())

        def addMatcher(f: $typeOfT => org.specs2.execute.Result): $matcherClassType = copy(Some((t: $typeOfT) => matcherFunction(t) and f(t)))

        def apply[S <: $typeOfT](s: org.specs2.matcher.Expectable[S]) = {
          val r = matcherFunction(s.value)
          val message = r.mapMessage(m => "For "+s.description+"\n"+m).message
          result(r.isSuccess, message, message, s)
        }

        ..$fieldMatchers
        ..$fieldValueMatchers
        ..$fieldFunctionMatchers
      }
    """
    (matcherClassType, classDefinition)
  }

  private def isConstructor(c: Context) = { import c.universe._
    (s: Symbol) =>  s match {
      case m: c.universe.MethodSymbol => m.isConstructor
      case other                      => false
    }
  }

  private def isSynthetic(c: Context) = { import c.universe._
    (s: Symbol) => s.isSynthetic
  }

  private def isGetter(c: Context) = { import c.universe._
    (s: Symbol) =>  s match {
      case m: c.universe.MethodSymbol => m.isGetter
      case other                      => false
    }
  }

}