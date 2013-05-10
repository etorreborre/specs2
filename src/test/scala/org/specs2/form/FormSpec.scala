package org.specs2
package form

import specification._
import Forms._
import matcher._
import execute._
import main.ArgumentsArgs

class FormSpec extends script.Specification with ResultMatchers with Grouped with XmlMatchers { def is = s2"""

A Form is a generic table which has an optional title and rows. Each row contains cells which can be created from
Fields, Props or other Forms.

A Form is usually created in a specification with expected values so that it can be displayed with the rest of the text
(whereas a DataTable is only displayed when there are failures. It is then "implemented" with actual values in an example.
Upon execution a Form will return a Result value summarizing the execution of each Prop it embeds.
                                                                                                                        
                                                                                                                        
A Form can be created                                                                                                 
  + with a title
  + with one field on one row
  + with two fields on one row
  + with a title and one field on each row
  + with a property on one row
  + with another Form on one row
  + with a seq of fields on one row
  + with a seq of fields on one row - and no title
  + with tabs
  + with a seq of Rows

  from a DataTable
    + the Form header is the DataTable header
    + with an additional column for failure messages
    + the form rows are the DataTable rows

A Form can be displayed, showing expected values
  with its title
    + if present: | title |
    + if absent: the text is empty

  the columns must be aligned for
    + a simple form with 2 fields $address1
    + a simple form with 3 fields, 2 on one row and one on the second row $address2
    + a form with more fields and rows $address3
    + a form with an inlined form $address4

  with one row only
    + and one cell
    + and 2 cells

  with a title and one row
    + and one cell
    + and 2 cells

A Form can be executed as a success
  + then its rows are a success
  + and row cells are a success

A Form can be executed as a failure
  + then its rows are a failure
  + and row cells are a failure

 A Form can be executed
   + then all its rows are executed

Forms rows and cells have equals/hashcode methods
  + row1 == row1
  + cell1 == cell1

Forms can be displayed as xhtml
  + the title must span all columns

A form can be added to another
  + inlined
                                                                                                            """

  "creation" - new group with datatables {
    eg := Form("title").title must_== Some("title")
    eg := Form.tr(field("name", "eric")).rows.size must_== 1
    eg := Form.tr(field("name", "eric"), field("age", 18)).rows.size must_== 1
    eg := Form.tr(field("name", "eric")).
                  tr(field("age", 18)).rows.size must_== 2
    eg := Form("title").tr(prop("name", "eric")).rows.size must_== 1
    eg := Form("title").tr(form("title")).rows.size must_== 1
    eg := Form("title").tr(Row.tr(field(1), field(2))).rows(0).cells.size must_== 2
    eg := Form.tr(Row.tr(field(1), field(2))).rows(0).cells.size must_== 2
    eg := Form.tabs(Seq("name")) { (s: String) => tab(s, Form("title")) }.rows.size must_== 1
    eg := Form.trs(Seq("a", "b")) { (s: String) => Row.tr(field(s)) }.rows.size must_== 2
  }
  "creation" - new group with datatables {
    eg := Form(okDataTable.decorator).text must startWith("| a | b |")
    eg := Form(koDataTable.decorator).text must contain("| a | b | message")
    eg := Form(okDataTable.decorator).text must contain("| 1 | 1 |")
  }

  val name = field("name", "eric")
  val age  = field("age", 18)

  "display" - new group {
    eg := form("title").text must_== "| title |"
    eg := Form().text must_== ""
  }
  "display" - new group {
    eg := Form.tr(name).text must_== "| name: eric |"
  }
  "display" - new group {
    eg := Form.tr(name, age).text must_== "| name: eric | age: 18 |"
  }
  "display" - new group {
    eg := form("title").tr(name).text must_==
          "| title      |\n" +
          "| name: eric |"
  }
  "display" - new group {
    eg := form("title").tr(name, age).text must_==
             "| title                |\n" + 
             "| name: eric | age: 18 |"

  }
  "display" - new group {
    eg := compare(address1,
      "| Address               |",
      "| street: Rose Crescent |",
      "| number: 2             |")

    eg := compare(address2,
      "| Address                           |",
      "| street: Rose Crescent | number: 2 |",
      "| town: Mosman                      |")

  }
  "display" - new group {
    eg := compare(address3,
      "| street: Rose Crescent | number: 2                         |",
      "| town: Mosman          | street: Rose Crescent | number: 2 |",
      "| town: Mosman                                              |")

    eg := compare(address4,
      "| town: Mosman          |",
      "| Address               |",
      "| street: Rose Crescent |",
      "| number: 2             |")

  }

  "execution" - new group {
    eg := Form.tr("a").setSuccess.execute must_== success
    eg := Form.tr("a").setSuccess.rows.forall(_.execute.isSuccess) must beTrue
  }

  "execution" - new group {
    eg := Form.tr("a").setFailure.execute.message must_== failure.message
    eg := Form.tr("a").setFailure.rows.forall(_.execute.isSuccess) must beFalse
  }

  "execution" - new group {
    eg := Form.tr(prop("a")("b")).
               tr(prop("a")("a")).
               tr(prop("c")("d")).executeForm.rows.filter(_.execute.isFailure) must have size(2)
  }

  "equality" - new group {
    eg := Row.tr(TextCell("a")) must_== Row.tr(TextCell("a"))
    eg := TextCell("a") must_== TextCell("a")
 }

  "xhtml" - new group {
    // count 3 per prop or field
    eg := Xml.colnumber(new FormCell(Form.th("title").
            tr(field(1)).
            tr(field("n", "v"), field("n", "v")).
            tr(prop("p", 1)(2)))) must_== 6
  }

  "nested forms" - new group {
    eg := Form("title").tr(Form.tr("hello").inline).toXml must ==/(
             <form>
               <table class="dataTable">
                 <tr><th colspan="4">title</th></tr><tr><div colspan="3"><tr><td style="" class="info" colspan="3">hello</td></tr></div></tr>
               </table>
             </form>)
  }

  def compare(form: String, expected: String*) = form must_== expected.mkString("\n", "\n", "\n")

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
