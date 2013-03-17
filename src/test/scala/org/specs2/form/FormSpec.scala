package org.specs2
package form

import specification._
import Forms._
import matcher._
import execute._
import main.ArgumentsArgs

class FormSpec extends Specification with ResultMatchers with Grouped { def is = s2"""

A Form is a generic table which has an optional title and rows. Each row contains cells which can be created from
Fields, Props or other Forms.

A Form is usually created in a specification with expected values so that it can be displayed with the rest of the text
(whereas a DataTable is only displayed when there are failures. It is then "implemented" with actual values in an example.
Upon execution a Form will return a Result value summarizing the execution of each Prop it embeds.
                                                                                                                        
                                                                                                                        
A Form can be created                                                                                                 
  with a title                                                                                              ${g1.e1}
  with one field on one row                                                                                 ${g1.e2}
  with two fields on one row                                                                                ${g1.e3}
  with a title and one field on each row                                                                    ${g1.e4}
  with a property on one row                                                                                ${g1.e5}
  with another Form on one row                                                                              ${g1.e6}
  with a seq of fields on one row                                                                           ${g1.e7}
  with a seq of fields on one row - and no title                                                            ${g1.e8}
  with tabs                                                                                                 ${g1.e9}
  with a seq of Rows                                                                                        ${g1.e10}
  from a DataTable
    the Form header is the DataTable header                                                                 ${g1.e11}
    with an additional column for failure messages                                                          ${g1.e12}
    the form rows are the DataTable rows                                                                    ${g1.e13}

A Form can be displayed, showing expected values
  with its title
    if present: | title |                                                                                   ${g2.e1}
    if absent: the text is empty                                                                            ${g2.e2}

  the columns must be aligned for
    a simple form with 2 fields $address1                                                                   ${g2.e3}
    a simple form with 3 fields, 2 on one row and one on the second row $address2                           ${g2.e4}
    a form with more fields and rows $address3                                                              ${g2.e5}
    a form with an inlined form $address4                                                                   ${g2.e6}

  with one row only
    and one cell                                                                                            ${g2.e7}
    and 2 cells                                                                                             ${g2.e8}

  with a title and one row
    and one cell                                                                                            ${g2.e9}
    and 2 cells                                                                                             ${g2.e1}

A Form can be executed as a success
  then its rows are a success                                                                               ${g3.e1}
  and row cells are a success                                                                               ${g3.e2}

A Form can be executed as a failure
  then its rows are a failure                                                                               ${g3.e3}
  and row cells are a failure                                                                               ${g3.e4}

 A Form can be executed
   then all its rows are executed                                                                           ${g3.e5}

Forms rows and cells have equals/hashcode methods
  row1 == row1                                                                                              ${g4.e1}
  cell1 == cell1                                                                                            ${g4.e2}

Forms can be displayed as xhtml
  the title must span all columns                                                                           ${g5.e1}

A form can be added to another
    inlined                                                                                                 ${g6.e1}
                                                                                                            """

  "creation" - new g1 with datatables {
    e1 := Form("title").title must_== Some("title")
    e2 := Form.tr(field("name", "eric")).rows.size must_== 1
    e3 := Form.tr(field("name", "eric"), field("age", 18)).rows.size must_== 1
    e4 := Form.tr(field("name", "eric")).
                  tr(field("age", 18)).rows.size must_== 2
    e5  := Form("title").tr(prop("name", "eric")).rows.size must_== 1
    e6  := Form("title").tr(form("title")).rows.size must_== 1
    e7  := Form("title").tr(Row.tr(field(1), field(2))).rows(0).cells.size must_== 2
    e8  := Form.tr(Row.tr(field(1), field(2))).rows(0).cells.size must_== 2
    e9  := Form.tabs(Seq("name")) { (s: String) => tab(s, Form("title")) }.rows.size must_== 1
    e10 := Form.trs(Seq("a", "b")) { (s: String) => Row.tr(field(s)) }.rows.size must_== 2

    e11 := Form(okDataTable.decorator).text must startWith("| a | b |")
    e12 := Form(koDataTable.decorator).text must contain("| a | b | message")
    e13 := Form(okDataTable.decorator).text must contain("| 1 | 1 |")

  }

  "display" - new g2 {
    val name = field("name", "eric")
    val age  = field("age", 18)

    e1 := form("title").text must_== "| title |"
    e2 := Form().text must_== ""
    e3 := Form.tr(name).text must_== "| name: eric |"

    e4 := Form.tr(name, age).text must_== "| name: eric | age: 18 |"
    e5 := form("title").tr(name).text must_==
          "| title      |\n" +
          "| name: eric |"

    e6 := form("title").tr(name, age).text must_==
             "| title                |\n" + 
             "| name: eric | age: 18 |"

    e7 := compare(address1,
      "| Address               |",
      "| street: Rose Crescent |",
      "| number: 2             |")

    e8 := compare(address2,
      "| Address                           |",
      "| street: Rose Crescent | number: 2 |",
      "| town: Mosman                      |")

    e9 := compare(address3,
      "| street: Rose Crescent | number: 2                         |",
      "| town: Mosman          | street: Rose Crescent | number: 2 |",
      "| town: Mosman                                              |")

    e10 := compare(address4,
      "| town: Mosman          |",
      "| Address               |",
      "| street: Rose Crescent |",
      "| number: 2             |")

    def compare(form: String, expected: String*) = form must_== expected.mkString("\n", "\n", "\n")
  }

  "execution" - new g3 {
    e1 := Form.tr("a").setSuccess.execute must_== success
    e2 := Form.tr("a").setSuccess.rows.forall(_.execute.isSuccess) must beTrue
    e3 := Form.tr("a").setFailure.execute.message must_== failure.message
    e4 := Form.tr("a").setFailure.rows.forall(_.execute.isSuccess) must beFalse
    e5 := Form.tr(prop("a")("b")).
               tr(prop("a")("a")).
               tr(prop("c")("d")).executeForm.rows.filter(_.execute.isFailure) must have size(2)
  }

  "equality" - new g4 {
    e1 := Row.tr(TextCell("a")) must_== Row.tr(TextCell("a"))
    e2 := TextCell("a") must_== TextCell("a")
 }

  "xhtml" - new g5 {
    // count 3 per prop or field
    e1 := Xml.colnumber(new FormCell(Form.th("title").
            tr(field(1)).
            tr(field("n", "v"), field("n", "v")).
            tr(prop("p", 1)(2)))) must_== 6
  }

  "nested forms" - new g6 {
    e1 := Form("title").tr(Form.tr("hello").inline).toXml must ==/(
             <form>
               <table class="dataTable">
                 <tr><th colspan="4">title</th></tr><tr><div colspan="3"><tr><td style="" class="info" colspan="3">hello</td></tr></div></tr>
               </table>
             </form>)
  }

  trait datatables extends DataTables {
    val okDataTable =
      "a" | "b" |>
        1  ! 1   | { (a, b) => a must_== b }

    val koDataTable =
      "a" | "b" |>
        1  ! 2   | { (a, b) => a must_== b }
  }

  lazy val street = field("street", "Rose Crescent")
  lazy val number = field("number", 2)
  lazy val town = field("town", "Mosman")

  lazy val address = Form("Address").
                       tr(street).
                       tr(number)

  lazy val address1 =  quote(address.text)

  lazy val address2 = quote(Form("Address").
                              tr(street, number).
                              tr(town).text)

  lazy val address3 = quote(Form.
                              tr(street, number).
                              tr(town, street, number).
                              tr(town).text)

  lazy val address4 = quote(Form.
                       tr(town).
                       tr(address.inline).text)

  def quote(s: String) = "\n"+s+"\n"
}
