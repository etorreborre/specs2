package org.specs2
package form

import specification.*
import Forms.{given, *}
import matcher.*

class FormSpec extends Specification with ResultMatchers with XmlMatchers {
  def is = s2"""

A Form is a generic table which has an optional title and rows. Each row contains cells which can be created from
Fields, Props or other Forms.

A Form is usually created in a specification with expected values so that it can be displayed with the rest of the text
(whereas a DataTable is only displayed when there are failures. It is then "implemented" with actual values in an example.
Upon execution a Form will return a Result value summarizing the execution of each Prop it embeds.

Creation
========

A Form can be created
  with a title                                   $creation1
  with one field on one row                      $creation2
  with two fields on one row                     $creation3
  with a title and one field on each row         $creation4
  with a property on one row                     $creation5
  with another Form on one row                   $creation6
  with a seq of fields on one row                $creation7
  with a seq of fields on one row - and no title $creation8
  with tabs                                      $creation9
  with a seq of Rows                             $creation10

  from a DataTable
    the Form header is the DataTable header        $datatable1
    with an additional column for failure messages $datatable2
    the form rows are the DataTable rows           $datatable3

Display
=======

A Form can be displayed, showing expected values
  with its title
    if present: | title |                                                         $display1
    if absent: the text is empty                                                  $display2

  the columns must be aligned for
    a simple form with 2 fields $address1                                         $display3
    a simple form with 3 fields, 2 on one row and one on the second row $address2 $display4
    a form with more fields and rows $address3                                    $display5
    a form with an inlined form $address4                                         $display6

  with one row only
    and one cell                                                                  $display7
    and 2 cells                                                                   $display8

  with a title and one row
    + and one cell                                                                $display9
    + and 2 cells                                                                 $display10

Execution
=========

A Form can be executed as a success
  then its rows are a success       $execution1
  and row cells are a success       $execution2

A Form can be executed as a failure
  then its rows are a failure       $execution3
  and row cells are a failure       $execution4

A Form can be executed
  then all its rows are executed    $execution5

Other
=====

Forms rows and cells have equals/hashcode methods
  row1 == row1   $methods1
  cell1 == cell1 $methods2

Forms can be displayed as xhtml
  the title must span all columns $asHtml1

A form can be added to another
  inlined $inlined1

"""

  def creation1 = Form("title").title must ===(Some("title"))
  def creation2 = Form.tr(field("name", "eric")).rows.size must ===(1)
  def creation3 = Form.tr(field("name", "eric"), field("age", 18)).rows.size must ===(1)
  def creation4 = Form.tr(field("name", "eric")).tr(field("age", 18)).rows.size must ===(2)
  def creation5 = Form("title").tr(prop("name", "eric")).rows.size must ===(1)
  def creation6 = Form("title").tr(form("title")).rows.size must ===(1)
  def creation7 = Form("title").tr(Row.tr(field(1), field(2))).rows(0).cells.size must ===(2)
  def creation8 = Form.tr(Row.tr(field(1), field(2))).rows(0).cells.size must ===(2)
  def creation9 = Form.tabs(Seq("name")) { (s: String) => Tabs().tab(s, Form("title")) }.rows.size must ===(1)
  def creation10 = Form.trs(Seq("a", "b")) { (s: String) => Row.tr(field(s)) }.rows.size must ===(2)

  import datables.*

  def datatable1 = Form(okDataTable.decorator).text must startWith("| a | b |")
  def datatable2 = Form(koDataTable.decorator).text must contain("| a | b | message")
  def datatable3 = Form(okDataTable.decorator).text must contain("| 1 | 1 |")

  val name = field("name", "eric")
  val age = field("age", 18)

  def display1 = form("title").text must ===("| title |")
  def display2 = Form().text must ===("")
  def display3 = Form.tr(name).text must ===("| name: eric |")
  def display4 = Form.tr(name, age).text must ===("| name: eric | age: 18 |")

  def display5 = form("title").tr(name).text must ===(
    "| title      |\n" +
      "| name: eric |"
  )

  def display6 = form("title").tr(name, age).text must ===(
    "| title                |\n" +
      "| name: eric | age: 18 |"
  )

  def display7 =
    compare(address1, "| Address               |", "| street: Rose Crescent |", "| number: 2             |")

  def display8 = compare(
    address2,
    "| Address                           |",
    "| street: Rose Crescent | number: 2 |",
    "| town: Mosman                      |"
  )

  def display9 = compare(
    address3,
    "| street: Rose Crescent | number: 2                         |",
    "| town: Mosman          | street: Rose Crescent | number: 2 |",
    "| town: Mosman                                              |"
  )

  def display10 = compare(
    address4,
    "| town: Mosman          |",
    "| Address               |",
    "| street: Rose Crescent |",
    "| number: 2             |"
  )

  def execution1 = Form.tr("a").setSuccess.execute must ===(success)
  def execution2 = Form.tr("a").setSuccess.rows.forall(_.execute.isSuccess) must beTrue

  def execution3 = Form.tr("a").setFailure.execute.message must ===(failure.message)
  def execution4 = Form.tr("a").setFailure.rows.forall(_.execute.isSuccess) must beFalse

  def execution5 =
    Form
      .tr(prop("a")("b"))
      .tr(prop("a")("a"))
      .tr(prop("c")("d"))
      .executeForm
      .rows
      .filter(_.execute.isFailure) must haveSize(2)

  def methods1 = Row.tr(TextCell("a")) must ===(Row.tr(TextCell("a")))
  def methods2 = TextCell("a") must ===(TextCell("a"))

  // count 3 per prop or field
  def asHtml1 = Xml.colnumber(
    new FormCell(Form.th("title").tr(field(1)).tr(field("n", "v"), field("n", "v")).tr(prop("p", 1)(2)))
  ) must ===(6)

  def inlined1 = Form("title").tr(Form.tr("hello").inline).toXml must ==/(<form>
       <table>
         <tr><th colspan="4">title</th></tr><tr><div colspan="3"><tr><td style="" class="info" colspan="3">hello</td></tr></div></tr>
       </table>
     </form>)

  // HELPERS
  def compare(form: String, expected: String*) = form must ===(expected.mkString("\n", "\n", "\n"))

  lazy val street = field("street", "Rose Crescent")
  lazy val number = field("number", 2)
  lazy val town = field("town", "Mosman")

  lazy val address = Form("Address").tr(street).tr(number)

  lazy val address1 = quote(address.text)

  lazy val address2 = quote(Form("Address").tr(street, number).tr(town).text)

  lazy val address3 = quote(Form.tr(street, number).tr(town, street, number).tr(town).text)

  lazy val address4 = quote(Form.tr(town).tr(address.inline).text)

  def quote(s: String) = "\n" + s + "\n"
}

trait datatables extends DataTables with MustMatchers:
  val okDataTable =
    "a" | "b" |>
      1 ! 1 | { (a, b) => a must ===(b) }

  val koDataTable =
    "a" | "b" |>
      1 ! 2 | { (a, b) => a must ===(b) }

object datables extends datatables
