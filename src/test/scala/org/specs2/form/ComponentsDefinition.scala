package org.specs2
package form
import Forms._

trait ComponentsDefinitions {
  case class Address(street: String, number: Int) {
    def form = fill(street, number)
    def fill(s: String, n: Int) = 
      Form("Address").
          tr(prop("street", s)(street)).
          tr(prop("number", n)(number))
  }
  case class Customer(name: String, address: Address) {
    def form = fill(name, address.form) 
    def fill(na: String, a: Form) =
      Form("Customer").
          tr(prop("name", na)(name)).
          tr(a)
         
  }
  case class initials(form: Form = Form.tr("First name", "Last name", "Initials")) {
    def computeInitials(f: String, l: String) = f(0).toUpper+"."+l(0).toUpper+"."
    
    def tr(firstName: String, lastName: String, expected: String) = initials {
      form.tr(firstName, lastName, prop(computeInitials(firstName, lastName))(expected))
    }
  }
  
  case class Order(lines: List[OrderLine] = Nil) {
    def line(orderLine: OrderLine) = Order(lines :+ orderLine)
    def form = fillSubset(lines:_*)
    def fillSubset(ls: OrderLine*) = Forms.form("Order", subset(lines, ls.toList))
    def fillSubsequence(ls: OrderLine*) = Forms.form("Order", subsequence(lines, ls.toList))
    def fillSet(ls: OrderLine*) = Forms.form("Order", set(lines, ls.toList))
    def fillSequence(ls: OrderLine*) = Forms.form("Order", sequence(lines, ls.toList))
  }
  case class OrderLine(name: String, quantity: Int) {
    def form = Form.tr(field("name", name), field("qty", quantity))
  }
}