package org.specs2
package reporter

import matcher.{StandardMatchResults, MustMatchers}
import specification._
import dsl.{AcceptanceDsl, FragmentsDsl}
import specification.create.{S2StringContext, DefaultFragmentFactory}
import control._
import text.Trim._
import execute._
import main.Arguments
import LineLogger._
import core._
import process.{Stats, DefaultExecutor, StatisticsRepository}
import io.StringOutput
import text.AnsiColors

class TextPrinterSpec extends Specification with tp { def is = s2"""

 The results of a specification can be printed as lines

   the title of the specification must be printed first       $a1
   regular text must have no status                           $a2

   a successful example must be displayed with a +            $b1
   a failed example must be displayed with a x                $b2
   an error example must be displayed with a !                $b3
   a skipped example must be displayed with a o               $b4
   a pending example must be displayed with a *               $b5
   a multi-line description must be indented ok               $b6
   if showtimes is true, each individual time must be shown   $b7

 Statistics must be displayed at the end   
   stats                                                      $c1

 Failure messages must be shown
   normal messages                                            $d1
   if failtrace then the stack trace must be shown            $d2
   with detailed failure                                      $d3

 Error messages must be shown
   with the exception message                                 $e1
   with the stacktrace                                        $e2

 Expected values must be shown
   when they are non empty                                    $f1

 Pending messages must be shown
   as PENDING if nothing is specified                         $g1
   as a specific user message                                 $g2

 Skipped messages must be shown
   as SKIPPED if nothing is specified                         $h1
   as the standard 'skipped'                                  $h2
   as a specific user message                                 $h3

 Formatting fragments are displayed
   breaks as 1 newline                                        $i1

  Specification links are displayed
    with no title                                             $j1
    with a title                                              $j2
    with results                                              $j3

 Fragments can be hidden by changing args
    xonly only show issues                                    $k1
    stats are not displayed with xonly when successful        $k2

 Fragments must be displayed in their creation order
    as soon as computed                                       $l1
"""
}

trait tp extends MustMatchers with StandardMatchResults with StandardResults with S2StringContext with AcceptanceDsl {
  import TextPrinterSpec._
  val factory = fragmentFactory; import factory._

  def a1 =
    "title".title ^ "" contains
      """|[info] title"""

  def a2 = "title".title ^ s2"""
presentation
""" contains
    """|[info] title
       |[info]
       |[info] presentation"""

  def b1 = s2"""
presentation
  e1 $ok
  e2 $ok
""" contains
    """|[info] presentation
       |[info]   + e1
       |[info]   + e2"""

  def b2 = s2"""
presentation
  e1 $ok
  e2 $ko
""" contains
    """|[info] presentation
       |[info]   + e1
       |[error]   x e2"""

  def b3 = s2"""e1 ${Error("ouch")}""" contains """[error] ! e1"""

  def b4 = s2"""e1 ${skipped("for now")}""" contains """[info] o e1"""
  def b5 = s2"""e1 ${pending("for now")}""" contains """[info] * e1"""

  def b6 = "t".title ^ "e1\nexample1" ! ok contains
  """|[info] + e1
     |[info]   example1""".stripMargin

  def b7 = {
    def ex1 = AsResult { Thread.sleep(30); ok }
    Arguments("showtimes") ^ s2"""e1 $ex1""" matches """(?s).*\[info\] \+ e1 \(\d\d.+\).*"""
  }

  def c1 =
s2"""e1 $ok
e2 $ko
e3 $ko
""" contains(
    """|[info] Total for specification TextPrinterSpec
       |[info] Finished in x ms
       |[info] x examples, x failures, x error""", (_:String).replaceAll("\\d+", "x"))

  def d1 =
s2"""e1 ${1 must_== 2}""" contains
      """|[error] x e1
         |[error]  '1' is not equal to '2'"""

  def d2 = Arguments.split("failtrace fullstacktrace") ^
s2"""e1 ${1 must_== 2}""" contains
     """|[error]_org.specs2.report"""

  def d3 =
s2"""e1 ${"abcdeabcdeabcdeabcdeabcde" must_== "adcdeadcdeadcdeadcdeadcde"}""" contains
    """|[error] Actual:   a[b]cdea[b]cdea[b]cdea[b]cdea[b]cde
       |[error] Expected: a[d]cdea[d]cdea[d]cdea[d]cdea[d]cde"""

  def e1 = Arguments("fullstacktrace") ^
    s2"""e1 $error1""" contains
      """|[error] ! e1
         |[error]  boom"""

  def e2 = s2"""e1 $error1""" contains """|[error] org.specs2.report"""

  def f1 = s2"""e1 ${Success("ok", "expected")}""" contains
    """|[info] + e1
       |[info] expected"""

  def g1 = s2"""e1 $pending""" contains
    """|[info] * e1 PENDING"""

  def g2 = s2"""e1 ${Pending("todo")}""" contains
    """|[info] * e1 todo"""

  def h1 = s2"""e1 ${Skipped("")}""" contains
    """|[info] o e1
       |[info] SKIPPED"""

  def h2 = s2"""e1 $skipped""" contains
    """|[info] o e1
       |[info] skipped""".stripMargin

  def h3 = s2"""e1 ${Skipped("wontdo")}""" contains
    """|[info] o e1
       |[info] wontdo"""

  def i1 = SpecStructure.create(SpecHeader(getClass), Arguments(), "e1" ! ok ^ break ^ break ^ "e2" ! ok) contains
    """|[info] + e1
       |[info]
       |[info] + e2"""

  def j1 = s2"""the ${SpecificationRef(SpecHeader(classOf[String]))} spec""" contains
    """|[info] the * String spec"""

  def j2 = s2"""the ${SpecificationRef(SpecHeader(classOf[String], Some("STRING")))} spec""" contains
    """|[info] the * STRING spec"""

  def j3 = {
    val repository = StatisticsRepository.memory
    repository.storeStatistics(classOf[String].getName, Stats(examples = 1, failures = 1)).runOption
    val env = Env().setStatisticRepository(repository)
    (s2"""the ${SpecificationRef(SpecHeader(classOf[String], Some("STRING")))} spec""", env) contains
    """|[info] the x STRING spec"""

  }

  def k1 = Arguments("xonly") ^ "title\n".title ^
    s2"""e1 $ok
         e2 $ko""" contains
    """|[info] title
       |[error] x e2"""

  def k2 = Arguments("xonly") ^ "title".title ^
    s2"""e1 $ok
         e2 $ok""" containsOnly """|[info] title"""

  def l1 = {
    val logger = new TestLogger

    val fragments = Fragments((1 to 100).flatMap(i => Seq(
      "ex"+i+"\n " ! {
        Thread.sleep(scala.util.Random.nextInt(100))
        logger.infoLine("executed "+i)
        ok
      })):_*)

    val spec = SpecStructure.create(SpecHeader(getClass, Some("title\n")), Arguments(), fragments)
    val env = Env(lineLogger = logger)
    TextPrinter.run(env)(DefaultExecutor.executeSpec(spec, env))

    val executed = logger.messages.filter(_.contains("executed")).map(_.replace("executed", "").trim.toInt)
    val printed = logger.messages.filter(_.contains("+")).map(_.replace("+", "").replace("ex", "").trim.toInt)

    "printed is sorted" ==> {
      printed must_== printed.sorted
    } and
    "executed is unsorted" ==> {
      executed must not be_==(executed.sorted)
    } and
    "the execution is mixed with the printing" ==> {
      val (l1, l2) = logger.messages.filter(s => s.contains("executed") || s.contains("+")).span(_.contains("executed"))
      l1.size aka (l1, l2).toString must not be_==(l2.size)
    }
  }
  /**
   * TEST METHODS
   */
  def error1 = { sys.error("boom"); ok }
}

object TextPrinterSpec extends MustMatchers with FragmentsDsl {

  implicit class fragmentOutputContains(fragment: Fragment) {
    def contains(contained: String, f: String => String = identity) = Fragments(fragment).contains(contained, f)
  }

  implicit class fragmentsOutputContains(fragments: Fragments) {
    def contains(contained: String, f: String => String = identity) =
      SpecStructure.create(SpecHeader(classOf[TextPrinterSpec]), Arguments(), fragments).contains(contained, f)
  }

  implicit class outputContains(spec: SpecStructure) {
    lazy val optionalEnv: Option[Env] = None

    lazy val printed = {
      val logger = stringLogger
      lazy val env =
        optionalEnv.fold(Env(lineLogger = logger,
          arguments = spec.arguments.overrideWith(Arguments.split("sequential fullstacktrace"))))(_.copy(lineLogger = logger))

      TextPrinter.run(env)(spec.setFragments(spec.fragments
        .prepend(DefaultFragmentFactory.break) // add a newline after the title
        .update(DefaultExecutor.execute(env))))

      val messages = logger.messages
      messages.map(_.removeEnd(" ")).mkString("\n").replace(" ", "_")
    }

    def contains(contained: String, f: String => String = identity) =
      f(printed) must contain(contained.stripMargin.replace(" ", "_"))

    def containsOnly(contained: String) =
      printed must be_==(contained.stripMargin.replace(" ", "_"))

    def startsWith(start: String) =
      printed must startWith(start.stripMargin.replace(" ", "_"))

    def matches(pattern: String) =
      printed must beMatching(pattern.stripMargin.replace(" ", "_"))
  }

  implicit class outputContainsForEnv(spec: (Fragments, Env)) extends outputContains(spec._1) {
    override lazy val optionalEnv = Some(spec._2)
  }
}

class TestLogger extends BufferedLineLogger with StringOutput {
  def infoLine(msg: String)    = super.append(AnsiColors.removeColors(msg))
  def errorLine(msg: String)   = ()
  def failureLine(msg: String) = ()
}
