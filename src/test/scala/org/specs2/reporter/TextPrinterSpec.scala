package org.specs2
package reporter

import scalaz.Scalaz
import Scalaz._
import text._
import AnsiColors._
import io.StringOutput
import main.Arguments
import execute._
import specification.{Example, SpecificationStructure, ExecutedFragment, Fragments, ExecutingSpecification}
import matcher.DataTables

class TextPrinterSpec extends Specification with DataTables { def is = s2"""
                                                                            
 The `TextPrinter` trait transforms a Seq of Executed Fragments to `PrintLines`
 and outputs them using a `TextResultOutput`.

 In the following examples the TextResultOutput is mocked so that the results are saved in
 a buffer of Strings.

 Arguments
 =========

 Several arguments can be used to modify the text presentation
   by default the Text and the examples are automatically indented                                ${prez().e1}
   if noindent = true then there is no automatic indenting                                        ${prez().e2}
   if xonly = true
     text is not shown                                                                            ${xonlyargs().e1}
     successful examples are not shown                                                            ${xonlyargs().e2}
     skipped examples are not shown                                                               ${xonlyargs().e3}
     pending examples are not shown                                                               ${xonlyargs().e4}
     failure examples are shown                                                                   ${xonlyargs().e5}
     error examples are shown                                                                     ${xonlyargs().e6}
     statistics are not shown                                                                     ${xonlyargs().e7}

   if showOnly = o
     text is not shown                                                                            ${skippedonlyargs().e1}
     successful examples are not shown                                                            ${skippedonlyargs().e2}
     skipped examples are shown                                                                   ${skippedonlyargs().e3}
     pending examples are shown                                                                   ${skippedonlyargs().e4}
     failure examples are not shown                                                               ${skippedonlyargs().e5}
     error examples are not shown                                                                 ${skippedonlyargs().e6}
     statistics are not shown                                                                     ${skippedonlyargs().e7}

   if showOnly = 1
      statistics are shown                                                                        ${statsonlyargs().e1}

   if failtrace = false, only the location of a failure is shown
     with an acceptance spec                                                                      ${failtrace().e1}
     with a mutable spec                                                                          ${failtrace().e2_1}
     with a ScalaCheck mutable spec                                                               ${failtrace().e2_2}
     with a Mockito mutable spec                                                                  ${failtrace().e2_3}
   if failtrace = true, failures stacktraces are shown                                            ${failtrace().e3}
   if fullStacktrace = true, all error stacktraces are shown                                      ${traces().e1}
   if plan = true, nothing is executed                                                            ${planargs().e1}
   if sequential = false examples are executed concurrently                                       ${seq().e1}
   if sequential = true examples are executed sequentially                                        ${seq().e2}
   if isolated = false examples are sharing variables                                             ${isolate().e1}
   if isolated = true examples are not sharing variables                                          ${isolate().e2}
   if stopOnFail = true everything is skipped after the first failure                             ${stopOnFailargs().e1}
   if skipAll = true, everything is skipped                                                       ${skipAllargs().e1}

   if color = true, the text output is colorized
     text is white                                                                                ${color().e1}
     success status is green                                                                      ${color().e2}
     failures status is yellow                                                                    ${color().e3}
     errors status are red                                                                        ${color().e4}
     pending status is blue                                                                       ${color().e5}
     skipped status is cyan                                                                       ${color().e6}
     stats are blue                                                                               ${color().e7}
     colors can be redefined by passing a Colors object                                           ${color().e8}
     colors can be redefined by passing system properties                                         ${color().e9}
     colors can be redefined by passing command-line args                                         ${color().e10}
     the background color can be specified as being white
       then the color scheme is inverted                                                          ${color().e11}

   when doing equals comparisons, differences are shown
     the differences show up after the failure message                                            ${diffs().e1}
     the separators can be modified with diffs(separators='<>')                                   ${diffs().e2}
     the trigger size can be modified with diffs(triggerSize=30)                                  ${diffs().e3}
     the shorten size can be modified with diffs(shortenSize=10)                                  ${diffs().e4}
     the full strings can be shown on 2 lines with line numbers with diffs(full=true)             ${diffs().e5}
     they can be disabled with diffs(show = false)                                                ${diffs().e6}
     unless there are too many of them diffs(diffRatio=30)                                        ${diffs().e7}


 Examples presentation
 =====================

 regular text must have no status                                                                 ${status().e1}
 a successful example must be displayed with a +                                                  ${status().e2}
 a failed example must be displayed with a x                                                      ${status().e3}
 an error example must be displayed with a !                                                      ${status().e4}
 a skipped example must be displayed with a o                                                     ${status().e5}
 a pending example must be displayed with a *                                                     ${status().e6}
 a multi-line description must be indented ok                                                     ${status().e7}
 if showtimes is true, each individual time must be shown                                         ${status().e8}
 a datatable must
   be used as a description if the example description is empty (meaning it's an auto-example)    ${status().e9}
   have no description if failing/in error (because the result shows all)                         ${status().e10}
   be properly aligned
     when successful                                                                              ${status().e11}
     when failing                                                                                 ${status().e12}
     when in error                                                                                ${status().e13}


 Title
 =====================

 the title of a specification is displayed if it is different from the name                       ${specTitle().e1}
                                                                                                  """

  implicit val default = Arguments()
  val t1         = "t1"
  val ex1        = "e1" ! success
  val ex2        = "e2" ! success
  val fail3      = "fail3" ! failure
  val error4     = "error4" ! anError
  val skipped5   = "skip it" ! skipped
  val pending6   = "todo" ! pending
  val bigString1 = "abcdefghijklmnopqrstuvwxyz"
  val bigString2 = "abcdefghijklnmopqrstuvwxyz"
  val bigFail    = "with diffs" ! { bigString1 must_== bigString2 }

  val tOk    = "a" | "b" |> 1 ! 1 | { (a, b) => a must_== b }
  val tKo    = "a" | "b" |> 1 ! 2 | { (a, b) => a must_== b }
  val tError = "a" | "b" |> 1 ! 2 | { (a, b) => throw new Exception("boom"); a must_== b }
  val tableOk: Example    = tOk
  val tableKo: Example    = tKo
  val tableError: Example = tError

  case class prez() {
    val noindent = args(noindent = true)
    
    def e1 = print(t1 ^ ex1 ^ ex2) must 
             contain("t1",
                     "+ e1",
                     "+ e2")
    
    def e2 = print(noindent ^ t1 ^ "  e1"!success ^ " e2"! success) must 
             contain("t1",
                     "+ e1",
                     "+ e2")
  }

  case class xonlyargs() {
    val arguments: Arguments = xonly

    def e1 = print(arguments ^ t1 ^ ex1 ^ fail3) must not containMatch("t1")
    def e2 = print(arguments ^ t1 ^ ex1 ^ fail3) must not containMatch("e1")
    def e3 = print(arguments ^ t1 ^ skipped5 ^ fail3) must not containMatch("skip it")
    def e4 = print(arguments ^ t1 ^ pending6 ^ fail3) must not containMatch("todo")
    def e5 = print(arguments ^ t1 ^ ex1 ^ fail3) must containMatch("fail3")
    def e6 = print(arguments ^ t1 ^ ex1 ^ error4) must containMatch("error4")
    def e7 = print(arguments ^ t1 ^ ex1 ^ ex2) must not containMatch("examples")
  }

  case class skippedonlyargs() {
    val arguments: Arguments = showOnly("o")

    def e1 = print(arguments ^ t1 ^ ex1 ^ fail3) must not containMatch("t1")
    def e2 = print(arguments ^ t1 ^ ex1 ^ fail3) must not containMatch("e1")
    def e3 = print(arguments ^ t1 ^ skipped5 ^ fail3) must containMatch("skip it")
    def e4 = print(arguments ^ t1 ^ pending6 ^ fail3) must not containMatch("todo")
    def e5 = print(arguments ^ t1 ^ ex1 ^ fail3) must not containMatch("fail3")
    def e6 = print(arguments ^ t1 ^ ex1 ^ error4) must not containMatch("error4")
    def e7 = print(arguments ^ t1 ^ ex1 ^ ex2) must not containMatch("examples")
  }

  case class statsonlyargs() {
    val arguments: Arguments = showOnly("1")

    def e1 = print(arguments ^ t1 ^ ex1 ^ ex2) must containMatch("examples")
  }

  case class color() {
    import text.AnsiColors._
    import text.Trim._
    
    def e1 = printWithColors(t1) must containMatch(white.remove("\033["))
    def e2 = printWithColors(ex1) must containMatch(green.remove("\033["))
    def e3 = printWithColors(fail3) must containMatch(yellow.remove("\033["))
    def e4 = printWithColors(error4) must containMatch(red.remove("\033["))
    def e5 = printWithColors(pending6) must containMatch(blue.remove("\033["))
    def e6 = printWithColors(skipped5) must containMatch(cyan.remove("\033["))
    def e7 = printWithColors(t1) must containMatch(blue.remove("\033["))

    def failureMustBeMagenta(cs: Colors) = printWithColors(colors(cs) ^ fail3) must containMatch("35m")

    def e8  = failureMustBeMagenta(new ConsoleColors { override val failureColor = magenta })
    def e9  = failureMustBeMagenta(SmartColors.fromArgs("failure:m"))
    def e10 = failureMustBeMagenta(new SmartColors {
      override lazy val properties = Map("color.failure"->"magenta")
    })
    def e11  = SmartColors.fromArgs("whitebg,success:b").textColor must_== black
  }

  case class diffs() {
    def test = bigString1 must_== bigString2
    def e1 = print(bigFail) must containMatch("hijkl\\[mn\\]opqrs")
    def e2 = print(diffs(separators="<>") ^ bigFail) must containMatch("...jkl<mn>opq...")
    def e3 = print(diffs(triggerSize=25) ^ bigFail) must containMatch("[mn]")
    def e4 = print(diffs(shortenSize=3) ^ bigFail) must containMatch("\\.jkl\\[mn\\]opq\\.")
    def e5 = print(diffs(full=true) ^ bigFail) must containMatch("jklmnopq")
    def e6 = print(diffs(show=false) ^ bigFail) must not containMatch("kl[mn]op")
    def e7 = print(diffs(show=true) ^ "" ! {bigString1 must_== bigString2.reverse} ) must not containMatch("\\[")
  }

  case class failtrace() {
    val failtrace: Arguments = args.report(failtrace = true)
    def e1   = print((new user.reporter.AcceptanceSpecification).content) must containMatch("AcceptanceSpecification.scala:11")
    def e2_1 = print((new user.reporter.MutableSpecification).content) must containMatch("MutableSpecification.scala:7")
    def e2_2 = print((new user.reporter.MutableScalaCheckSpecification).content) must containMatch("MutableSpecification.scala:14")
    def e2_3 = print((new user.reporter.MutableMockitoSpecification).content) must containMatch("MutableSpecification.scala:20")
    def e3 = print(fullStackTrace <| failtrace ^ t1 ^ ex1 ^ fail3) must containMatch("org.specs2")
  }

  case class traces() {
    def e1 = print(fullStackTrace ^ t1 ^ ex1 ^ "e" ! {throw new Exception("ouch"); ok}) must containMatch("org.specs2")
  }

  case class planargs() {
    val plan: Arguments = args(plan = true)
    def e1 = print(plan ^ t1 ^ ex1 ^ fail3) must contain("* e1") and not containMatch("\\+ e1") 
  }

  case class skipAllargs() {
    val sk: Arguments = args(skipAll = true)
    def e1 = {
      val spec = print(sk ^ t1 ^ ex1 ^ fail3)
      (spec must containMatch("o e1")) and
      (spec must contain("o fail3"))
    }
  }

  case class seq() {
    val messages = new StringOutput {}
    val slowex1 = "e1" ! { Thread.sleep(500); messages.println("e1"); success }
    val fastex2 = "e2" ! { Thread.sleep(10); messages.println("e2"); success }
    val fastex3 = "e3" ! { Thread.sleep(10); messages.println("e3"); success }

    def e1 = {
      print(fastex3 ^ slowex1 ^ fastex2 ^ fastex3)
      messages.messages must contain("e3", "e1").inOrder.orSkip("this example might fail sometimes")
    }
    def e2 = {
      print(args(sequential = true) ^ slowex1 ^ fastex2)
      messages.messages must contain("e1", "e2").inOrder
    }
  }
  case class isolate() {
    def e1 = print(new NonIsolatedSpecification) must contain("+ e1", "x e2")
    def e2 = print(new IsolatedSpecification) must contain("+ e1", "+ e2")
  }
  case class stopOnFailargs() {
    def e1 = {
      print(sequential ^ stopOnFail ^
            "ok" ! success ^
            "ko" ! Failure("fail") ^
            "ok2" ! success) must contain("o ok2")
    }
  }

  case class status() {
    def e1 = print(t1 ^ ex1) must containMatch("^\\s*t1") 
    def e2 = print(t1 ^ ex1) must contain("+ e1") 
    def e3 = print(t1 ^ fail3) must contain("x fail3") 
    def e4 = print(t1 ^ error4) must contain("! error4") 
    def e5 = print(t1 ^ skipped5) must contain("o skip it") 
    def e6 = print(t1 ^ pending6) must containMatch("\\* todo") 
    def e7 = print(t1 ^ "e1\nexample1" ! success) must contain(
        "+ e1",
        "  example1") 
    def e8 = print(args.report(showtimes=true) ^ t1 ! success) must containMatch("t1 \\(.*\\)")

    def e9 = print(t1 ^ tableOk) must contain("+ a | b")
    def e10 = print(t1 ^ tableKo) must contain("x ")
    def e11 = print(t1 ^ tableOk) must contain("+ a | b",
                                               "  1 | 1")
    def e12 = print(t1 ^ tableKo) must contain("x ",
                                               "  | a | b |",
                                               "x | 1 | 2 | '1' is not equal to '2'") ^^ ((s1: String, s2: String) => s1.startsWith(s2))
    def e13 = print(t1 ^ tableError) must contain("! ",
                                                  "  | a | b |",
                                                  "! | 1 | 2 | boom") ^^ ((s1: String, s2: String) => s1.startsWith(s2))
  }

  case class specTitle() {
    def e1 = print("a title".title ^ ex1) must contain("a title")
  }

  def print(specification: SpecificationStructure): Seq[String] =
    printWithColors(specification).map(removeColors(_))

  def print(fragments: Fragments): Seq[String] =
    printWithColors(fragments).map(removeColors(_))

  def printWithColors(fs: Fragments): Seq[String] =
    preReporter.exec(fs).foreach((n, fs) => printer.print(fs))

  def printWithColors(specification: SpecificationStructure): Seq[String] =
    preReporter.exec(specification).foreach((n, fs) => printer.print(fs))

  val outer = this
  def printer = new TextPrinter {
    override lazy val textOutput = new TextResultOutput with StringOutput
    def print(fs: Seq[ExecutedFragment]) = {
      super.print(outer.content.specName, fs)
      textOutput.messages
    }
  }

  def preReporter = new ConsoleReporter {

    def exec(fs: Fragments): ExecutingSpecification = exec(new Specification { def is = fs })

    def exec(specification: SpecificationStructure): ExecutingSpecification = {
      val args = specification.is.arguments
      specification |> select(args) |> sequence(args) |> execute(args)
    }
  }
}

class IsolatedSpecification extends IsolableSpecification(true)
class NonIsolatedSpecification extends IsolableSpecification(false)

case class IsolableSpecification(isolate: Boolean) extends mutable.Specification {
  sequential
  if (isolate) isolated

  "some examples" >> {
    var i = 0
    "e1" in { i = i+1; i must_== 1 }
    "e2" in { i = i+1; i must_== 1 }
  }
}


