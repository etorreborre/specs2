package org.specs2
package guide
import sys._
import form._
import _root_.examples.Address

class Forms extends UserGuidePage with specification.Forms { def is = s2"""

Forms are a way to represent domain objects or services, and declare expected values in a tabular format. Forms can be designed as reusable pieces of specification where complex forms can be built out of simple ones.

Forms are built by creating `Fields` or `Props` and placing them on rows. The following examples show, by order of complexity, the creation of:

  1. fields
  1. effects
  1. properties
  1. a simple Form using properties
  1. a simple Address entity encapsulating the above form
  1. a composite Customer entity using the Address instance
  1. a decision table having some related columns
  1. a composite Order - OrderLine entity (1-n) relationship

 For all the code samples below you need to extend the `org.specs2.specification.Forms` trait.

### Fields

A `Field` is simply a label and a value. It is used in forms to display regular information. You can create a `Field` with these methods:

  * `field(value)`: creates a field for a value, where the label is empty
  * `field(label, value)`: creates a field with a label and a value
  * `field(label, field1, field2, ...)`: creates a field with a label and values coming from other fields

 When the form is displayed, here's how the fields are displayed:


  ${form("Fields").
     tr(field("code").bold.center, field("is displayed as").bold.center).
     tr(field("field(\"value\")").code, field("value")).
     tr(field("field(\"label\", \"value\")").code, field("label", "value")).
     tr(field("field(\"label\", field(\"value1\"), field(\"value2\"))").code, field("label", field("value1"), field("value2")))}

In terms of execution, the value is only evaluated when the `Field` is executed (when executing the parent Form for example). If an exception is thrown during that evaluation, the exception message will be displayed in place of the value.

### Effects

An `Effect` is almost like a `Field` but it never shows its value, unless there's an exception when executed. The value of an `Effect` is supposed to have some kind of side-effect, like clicking on a webpage, and only the effect label will be displayed (except when there is an exception, in that case the exception message is added). You can create an `Effect` with these methods:

  * `effect(value)`: creates an effect with no label
  * `effect(label, value)`: creates an effect with a label and a value that will be evaluated when the `Effect` is executed
  * `effect(effect1, effect2, ...)`: creates an effect with all the effects labels and a side-effect sequencing all side-effects

### Properties

A `Prop` is like a `Field`, it has a label. But you can give it 2 values, an "actual" one and an "expected" one. When executing the property, both values are compared to get a result. You can create a `Prop` with the following functions:

   * `prop(value)`: a property with no label
   * `prop(label, actual)`: a property with a label and an actual value
   * `prop(label, actual, expected)`: a property with a label, an actual value and an expected one
   * `prop(label, actual, constraint)`: a property with a label, an actual value and a function taking the actual value,
     an expected one and returning a `Result`

         `prop("label", "actual", (a: String, b: String) => (a === b).toResult)

   * `prop(label, actual, matcher constraint)`: a property with a label, an actual value and a function taking the expected value,
     returning a Matcher that will be applied to the actual one

         `prop("label", "expected", (s: String) => beEqualTo(s))

   * `prop(label, actual, matcher)`: a property with a label, an actual value and a matcher to apply to that value

         `prop("label", Some(1), beSome)

      If the matcher is `mute`d then no message will be displayed in case of a failure.

If the expected value is not provided when building the property, it can be given with the `apply` method:

        // apply "sets" the expected value
        prop.apply("expected")
        // or
        prop("expected")

Let's look at a few examples:

 ${form("Properties").
    tr(field("code").bold.center, field("is displayed as").bold.center).
    tr(field("prop(\"expected\")(\"expected\")").code, prop("expected")("expected")).
    tr(field("prop(\"label\", \"expected\", \"expected\")").code, prop("label", "expected", "expected")).
    tr(field("prop(\"label\", \"expected\")(\"expected\")").code, prop("label", "expected")("expected")).
    tr(field("prop(\"label\", \"actual\")(\"expected\")").code, prop("label", "actual")("expected")).
    tr(field("prop(\"label\", { error(\"but got an error\"); \"actual\" })(\"expected\")").code, prop("label", { error("but got an error"); "actual" })("expected")).
    tr(field("prop(\"label\", \"actual\", (a: String, b: String) => (a === b).toResult)(\"expected\")").code, prop("label", "expected", (a: String, b: String) => (a === b).toResult)("expected")).
    tr(field("prop(\"label\", \"actual\", (s: String) => beEqualTo(s))(\"expected\")").code, prop("label", "expected", (s: String) => beEqualTo(s))("expected")).
    tr(field("prop(\"label\", \"actual\", beEqualTo(\"expected\"))").code, prop("label", "actual", beEqualTo("expected"))).
    tr(field("prop(\"label\", \"actual\", beEqualTo(\"expected\").mute)").code, prop("label", "actual", beEqualTo("expected").mute)).executeForm.toXml.toString}

### Styles

Most of the time, the display of Fields and Properties can be left as it is but sometimes you want to style the output of labels and values. You can do this by using `decorateWith` and `styleWith` methods, or some equivalent shortcuts:

 ${form("Style").
    tr(field("code").bold.center, field("is displayed as").bold.center).
    tr(field("decorate with").bkGrey.bold).
    tr(field("field(\"label\", \"value\").decorateWith(f: Any => <em>{f}</em>)").code, field("label", "value").decorateWith((ns: Any) => <em>{ns}</em>)).
    tr(field("field(\"label\", \"value\").bold").code, field("label", "value").bold).
    tr(field("field(\"label\", \"value\").boldLabel").code, field("label", "value").boldLabel).
    tr(field("field(\"label\", \"value\").boldValue").code, field("label", "value").boldValue).
    tr(field("field(\"label\", \"value\").italics").code,      field("label", "value").italics).
    tr(field("field(\"label\", \"value\").italics.bold").code,      field("label", "value").italics.bold).
    tr(field("field(\"1 must_== 1\").code").code, field("1 must_== 1").code).
    tr(field("style with").bkGrey.bold).
    tr(field("field(\"label\", \"value\").styleWith(\"color\"->\"#FF1493\")").code, field("label", "value").styleWith("color"->"#FF1493")).
    tr(field("field(\"label\", \"value\").color(\"#FF1493\")").code, field("label", "value").color("#FF1493")).
    tr(field("field(\"label\", \"value\").bkColor(\"#FF1493\")").code, field("label", "value").bkColor("#FF1493")).
    tr(field("field(\"label\", \"value\").green").code, field("label", "value").green).
    tr(field("field(\"label\", \"value\").bkGreen").code, field("label", "value").bkGreen) }

 All the methods above, when named `xxx` are available as `xxxLabel` and `xxxValue` to do the formatting for the label or the value only. The available colors are:

 ${form("Colors").
    tr(field("name").bold.center, field("code").bold.center, field("color").bold.center).
    tr(field("white"), field("#FFFFFF"), field("").bkWhite).
    tr(field("blue"), field("#1E90FF"), field("").bkBlue).
    tr(field("red"), field("#FF9999"), field("").bkRed).
    tr(field("green"), field("#CCFFCC"), field("").bkGreen).
    tr(field("yellow"), field("#FFFF99"), field("").bkYellow).
    tr(field("grey"), field("#EEEEEE"), field("").bkGrey) }

### Simple form

Now that we know how to create Fields and Properties, creating a `Form` is as easy as putting them on separate lines:

        import org.specs2.form._
        import FormsBuilder._

         Form("Address").
           tr(prop("street", actualStreet(123), "Oxford St")).
           tr(prop("number", actualNumber(123), 20))

The form has a title `"Address"` and 2 properties, each one on a distinct row. The `actualStreet()` and `actualNumber()`
methods are supposed to retrieve the relevant values from a database.

In some cases (see the Calculator example below) you can create a header row using the `th` method:

  * `th(field("a"), field("b"))`
  * or `th("a", "b")` using an implicit conversion of Any => Field[Any]

Inserting the form in a Specification is also very simple, you just chain it with the `^` operator:

         class SpecificationWithForms extends Specification with Forms { def is =

           "The address must be retrieved from the database with the proper street and number" ^
            Form("Address").
              tr(prop("street", actualStreet(123), "Oxford St")).
              tr(prop("number", actualNumber(123), 20))                                        ^
                                                                                               end
         }

One way to encapsulate and reuse this Form across specifications is to define a case class:

         case class Address(street: String, number: Int) {
           def retrieve(addressId: Int) = {
            val address = actualAddress(addressId)
            Form("Address").
              tr(prop("street", address.street, street)).
              tr(prop("number", address.number, number))
           }
           def actualAddress(addressId: Int): AddressEntity  = ...
         }

And then you can use it like this:

         class AddressSpecification extends Specification with Forms { def is =
           "The address must be retrieved from the database with the proper street and number" ^
             Address("Oxford St", 20).                      /** expected values */
               retrieve(123)                                /** actual address id */           ^
                                                                                               end
         }

##### Adding several rows at once

A very practical way to add rows programmatically is to start from a seq of values and have a function creating a Row object
for each value:

        Form("a new Form").trs(addresses) { a: Address => Row.tr(field(a.number), field(a.street)) }

 ${ Form("a new Form").trs(addresses) { a: Address => Row.tr(field(a.number), field(a.street)) }  }

#### Nesting a Form into another Form

Forms can be composed of other Forms to display composite information:

        val address = Form("Address").
                       tr(field("street", "Rose Crescent")).
                       tr(field("number", 3))

        val person = Form("Person").
                       tr(field("name", "Eric")).
                       tr(address)

 ${ Form("Person").
    tr(field("name", "Eric")) }

This will be displayed with the address as a nested table inside the main one on the last row. However in some case, it's
preferable to have the rows of that Form to be included directly in the outer table. This can be done by *inlining* the
nesting Form:

        val person = Form("Person").
                       tr(field("name", "Eric")).
                       tr(address.inline)            // address is inlined

And the result is:

 ${ Form("Person").
     tr(field("name", "Eric")).
     tr(address.inline) }

#### Nesting a Form into an Effect or a Prop

When using Forms in specifications we can describe different levels of abstraction. If we consider the specification of
a website for example, we want to be able to use a Form having 2 rows and describing the exact actions to do on the Login
page:

          val loginForm = Form("login").
                            tr(effect("click on login", clickOn("login"))).
                            tr(effect("enter name",     enter("name", "me"))).
                            tr(effect("enter password", enter("password", "pw"))).
                            tr(effect("submit", submit))

 ${loginForm}

However in a "purchase" scenario we want all the steps above to represent the login actions as just one step. One way to
do this is to transform the login Form to an Effect or a Prop:

          Form("purchase").
            tr(loginForm.toEffect("login")).
            tr(selectForm.toEffect("select goods")).
            tr(checkTotalForm.toProp("the total must be computed ok").bkWhiteLabel)

If everything goes fine, the detailed nested form is not shown:

 ${ Form("purchase").
     tr(loginForm.toEffect("login")).
     tr(selectForm.toEffect("select goods")).
     tr(checkTotalForm.toProp("the total must be computed ok").bkWhiteLabel) }

Otherwise:

  * if the Form is embedded into an Effect, Errors will be reported
  * if the Form is embedded into a Prop, Failures will be reported, like that

${ Form("purchase").
    tr(loginForm.toEffect("login")).
    tr(selectForm.toEffect("select goods")).
    tr(checkKoTotalForm.toProp("the total must be computed ok").bkWhiteLabel).executeForm.toXml.toString }

#### Using tabs

If there are too many fields to be displayed on a Form you can use tabs:

        "A person can have 2 addresses"^
          Form("Addresses").tr {
            tab("home",
              Address("Oxford St", 12).
              fill("Oxford St", 12)).
            tab("work",
              Address("Rose Cr.", 3).
              fill("Rose Cr.", 3))
          }

The first `tab` call will create a `Tabs` object containing the a first tab with "home" as the title and an Address form
as its content. Then every subsequent `tab` calls on the `Tabs` object will create new tabs:


 ${ Form("Addresses").tr(
    tab("home", Form("Address").
                  tr(prop("street", "Oxford St")("Oxford St")).
                  tr(prop("number", 12)(12))).
    tab("work", Form("Address").
                  tr(prop("street", "Rose Cr.")("Rose Cr.")).
                  tr(prop("number", 2)(2)))) }

Tabs can also be created from a seq of values. Let's pretend we have a list of `Address` objects with a name and a Form
displaying the `Address` values. You can write:

      Form("Addresses").tabs(adresses) { address: Address => tab(address.name, address.form) }

### Aggregating forms

Now that we've defined a form for a simple entity, let's see how we can reuse it with a larger entity:

 * the Customer form defines a name attribute and embeds an instance of the Address form
 * it is defined by setting the name on one row and the Address form on the second row

    *[and for this example, we define a slightly different Address form]*

        case class Address(street: String, number: Int) {
          def actualIs(address: AddressEntity) = {
            Form("Address").
              tr(prop("street", address.street, street)).
              tr(prop("number", address.number, number))
          }
        }

        case class Customer(name: String, address: Address) {
          def retrieve(customerId: Int) = {
            val customer = actualCustomer(customerId)
            Form("Customer").
              tr(prop("name", customer.name)(name)).
              tr(address.actualIs(customer.address))
          }
          def actualCustomer(customerId: Int) = ... // fetch from the database
        }

        class CustomerSpecification extends Specification with Forms { def is =
          "The customer must be retrieved from the database with a proper name and address" ^
            Customer(name = "Eric",
                     address = Address(street = "Rose Crescent", number = 2)).
                     retrieve(123)                                                          ^
                                                                                            end
        }

As you also see above, named arguments can bring more readibility to the expected values.

#### Lazy cells

Fields, Props and Forms are added right away to a row when building a Form with the `tr` method. If it is necessary to add
them with a "call-by-name" behavior, the `lazyfy` method can be used:

            def address = ... // build an Address Form
            Form("Customer").
              tr(prop("name", customer.name)(name)).
              // the address Form will be built only when the Customer Form is rendered
              tr(lazyfy(address.actualIs(customer.address)))

#### Xml cells

Any xml can be "injected" on a row by using an `XmlCell`:

            Form("Customer").
              tr(prop("name", customer.name)(name)).
              tr(XmlCell(<div><b>this is a bold statement</b><div>))

### 1-n relationships

When there are 1 - n relationships between entities the situation gets bit more complex.

For example you can have an "Order" entity, which has several "OrderLines". In that case there are several things that
we might want to specify:

 * the expected rows are included in the actual rows, with no specific order (this is the usual case)
 * the expected rows are included in the actual rows, in the same order
 * the expected rows are exactly the actual rows, with no specific order
 * the expected rows are exactly the actual rows, in the same order

Let's see how to declare this. The 2 classes we're going to use are:

        import Form._
        import specification.Forms._

        case class Order(orderId: Int) {
          lazy val actualLines = // those should be extracted from the actual order entity retrieved by id
            OrderLine("PIS", 1) ::
            OrderLine("PS", 2) ::
            OrderLine("BS", 3) ::
            OrderLine("SIS", 4) ::
            Nil

          def base = form("Order").th("name", "qty")
          def hasSubset(ls: OrderLine*)      = base.subset(actualLines, ls)
          def hasSubsequence(ls: OrderLine*) = base.subsequence(actualLines, ls)
          def hasSet(ls: OrderLine*)         = base.set(actualLines, ls)
          def hasSequence(ls: OrderLine*)    = base.sequence(actualLines, ls)
        }

        case class OrderLine(name: String, quantity: Int) {
          def form = tr(field(name), field(quantity))
        }

The `OrderLine` class simply creates a form with 2 fields: name and quantity. The `Order` class is able to retrieve the
actual order entity (say, from a database) and to extract `OrderLine` instances. It also has several methods to build Forms
depending on the kind of comparison that we want to do.

#### Subset

`Form.subset` uses the `FormDiffs.subset(a, b)` method to calculate the differences between the lines of `a` and `b`:

 * lines existing in `a` but not `b` are left untouched
 * lines existing in `a` and `b` are marked as success
 * lines existing in `b` and not `a` are marked as failures

        Order(123).hasSubset {
          OrderLine("BS", 3),
          OrderLine("PIS", 1),
          OrderLine("TDGL", 5)
        }

This form returns:

${ Order(123).hasSubset (
    OrderLine("BS", 3),
    OrderLine("PIS", 1),
    OrderLine("TDGL", 5)
   ).executeForm.toXml.toString }

#### Subsequence

`Form.subsequence` uses the `FormDiffs.subsequence(a, b)` method to calculate the differences and add them to the Form:

 * lines existing in `a` but not `b` are left untouched
 * lines existing in `a` and `b` in the same order are marked as success
 * lines existing in `b` and not `a` are marked as failures
 * lines existing in `b` and `a` but out of order are marked as failures

        Order(123).hasSubsequence {
          OrderLine("PS", 2),
          OrderLine("BS", 3),
          OrderLine("PIS", 1),
          OrderLine("TDGL", 5)
        }

This form returns:

${ Order(123).hasSubsequence (
     OrderLine("PS", 2),
     OrderLine("BS", 3),
     OrderLine("PIS", 1),
     OrderLine("TDGL", 5)
   ).executeForm.toXml.toString }

#### Set

`Form.set` uses the `FormDiffs.set(a, b)` method to calculate the differences between the lines of `a` and `b`:

 * lines existing in `a` but not `b` are marked as failures
 * lines existing in `a` and `b` are marked as success
 * lines existing in `b` and not `a` are marked as failures

        Order(123).hasSet {
          OrderLine("BS", 3),
          OrderLine("PIS", 1),
          OrderLine("TDGL", 5)
        }

This form returns:

${ Order(123).hasSet (
     OrderLine("BS", 3),
     OrderLine("PIS", 1),
     OrderLine("TDGL", 5)
   ).executeForm.toXml.toString }

#### Sequence

`Form.sequence` uses the `FormDiffs.sequence(a, b)` method to calculate the differences between the lines of `a` and `b`:

 * lines existing in `a` but not `b` are marked as failures
 * lines existing in `a` and `b` in the right order are marked as success
 * lines existing in `b` and not `a` are marked as failures

        Order(123).hasSequence {
          OrderLine("PS", 2),
          OrderLine("BS", 3),
          OrderLine("PIS", 1),
          OrderLine("TDGL", 5)
        }

This form returns:

${ Order(123).hasSequence (
     OrderLine("PS", 2),
     OrderLine("BS", 3),
     OrderLine("PIS", 1),
     OrderLine("TDGL", 5)
   ).executeForm.toXml.toString }

### Decision tables

One very popular type of Forms are *decision tables*. A decision table is a Form where, on each row, several values are
used for a computation and the result must be equal to other values on the same row. A very simple example of this is a
calculator:

        import Form._

        case class Calculator(form: Form = Form()) {
          def tr(a: Int, b: Int, a_plus_b: Int, a_minus_b: Int) = Calculator {
            def plus = prop(a + b)(a_plus_b)
            def minus = prop(a - b)(a_minus_b)
            form.tr(a, b, plus, minus)
          }
        }
        object Calculator {
          def th(title1: String, titles: String*) = Calculator(Form.th(title1, titles:_*))
        }

The `Calculator` object defines a `th` method to create the first `Calculator` Form, with the proper title. The `th` method:

  * takes the column titles (there must be at least one title)
  * creates a header row on the form
  * returns a new Calculator containing this form (note that everything is immutable here)

The `Calculator` case class embeds a Form and defines a `tr` method which

  * takes actual and expected values
  * creates properties for the computations
  * creates a form with a new row containing those fields and properties
  * returns a new Calculator containing this form

And you use the `Calculator` Form like this:

         class CalculatorSpecification extends Specification with Forms { def is  =
           "A calculator must add and subtract Ints" ^
             Calculator.
               th("a", "b", "a + b", "a - b").
               tr(1,   2,   3,       -1     ).
               tr(2,   2,   4,       0      )
         }

 Here is the output:

${ Calculator.
     th("a", "b", "a + b", "a - b").
     tr(1, 2, 3, -1).
     tr(2, 2, 4, 0) }

And if something goes wrong:

${ Calculator.
     th("a", "b", "a + b", "a - b").
     tr(1, 2, 3, -1).
     tr(2, 2, 4, 2).form.executeForm.toXml.toString }

And when it goes *very* wrong (like throwing an `error("very wrong")`), there will be red cells and stacktraces:

${ WrongCalculator.
     th("a", "b", "a + b", "a - b").
     tr(1, 2, 3, -1).
     tr(2, 2, 4, 2).form.executeForm.toXml.toString }

Note that the Calculator class is not, in itself an Example. But there is an implicit definition automatically transforming
`Any { def form: Form }` to `Example` so that an explicit call to `.form` is not necessary in order to include the Form in the
specification.


  - - -
                                                                                                                        """ ^
                                                                                                                        br ^
  include(xonly, new CalculatorSpecification)                                                                           ^
  end

  import Form._
  import specification.Forms._

  case class Order(orderId: Int) {
    lazy val actualLines = // those should be extracted from the actual order entity retrieved by id
      OrderLine("PIS", 1) ::
      OrderLine("PS", 2) ::
      OrderLine("BS", 3) ::
      OrderLine("SIS", 4) ::
      Nil

    def base = form("Order").th("name", "qty")
    def hasSubset(ls: OrderLine*)      = base.subset(actualLines, ls)
    def hasSubsequence(ls: OrderLine*) = base.subsequence(actualLines, ls)
    def hasSet(ls: OrderLine*)         = base.set(actualLines, ls)
    def hasSequence(ls: OrderLine*)    = base.sequence(actualLines, ls)
  }

  case class OrderLine(name: String, quantity: Int) {
    def form = tr(field(name), field(quantity))
  }

  class CalculatorSpecification extends Specification with specification.Forms { def is  =
    "A calculator must add and subtract Ints" ^
      Calculator.
        th("a", "b", "a + b", "a - b").
        tr(1, 2, 3, -1).
        tr(2, 2, 4, 0)
  }

  case class Calculator(form: Form = Form()) {
    def tr(a: Int, b: Int, a_plus_b: Int, a_minus_b: Int) = Calculator {
      def plus = prop(a + b)(a_plus_b)
      def minus = prop(a - b)(a_minus_b)
      form.tr(a, b, plus, minus)
    }
  }
  object Calculator {
    def th(title1: String, titles: String*) = Calculator(Form.th(title1, titles:_*))
  }
  case class WrongCalculator(form: Form = Form()) {
    def tr(a: Int, b: Int, a_plus_b: Int, a_minus_b: Int) = WrongCalculator {
      def plus = prop(a + b)(a_plus_b)
      def minus = if (a_minus_b != 2) prop(a - b)(a_minus_b) else prop({error("very wrong"); 0})(a_minus_b)
      form.tr(a, b, plus, minus)
    }
  }
  object WrongCalculator {
    def th(title1: String, titles: String*) = WrongCalculator(Form.th(title1, titles:_*))
  }

  lazy val addresses = Seq(Address("Rose Crescent", 3), Address("Oxfort St", 4))

  val address = Form("Address").
                  tr(field("street", "Rose Crescent")).
                  tr(field("number", 3))

  val loginForm = Form("login").
                    tr(effect("click on login", "login")).
                    tr(effect("enter name",     "name")).
                    tr(effect("enter password", "password")).
                    tr(effect("submit", "submit"))

  val selectForm = Form("select").tr(field("selection step"))
  
  val checkTotalForm = Form("Check Total").
                         tr(effect("Compute total", 100)).
                         tr(prop("Total", 100)(100))

  val checkKoTotalForm = Form("Check Total").
                         tr(effect("Compute total", 100)).
                         tr(prop("Total", 100)(200))

}
