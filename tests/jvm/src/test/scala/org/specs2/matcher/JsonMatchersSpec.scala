package org.specs2
package matcher

class JsonMatchersSpec extends Specification with JsonMatchers {
  def is = s2"""

 The / matcher matches a name and a value if the input is a Map
 ${"{'name' : 'Joe'}" must /("name" -> anyValue)}
 ${"{'name' : 'Joe'}" must /("name" -> "Joe")}
 ${"{'name' : 'Joe'}" must /("name" -> contain("o"))}
 ${"{'age'  : '33' }" must /("age" -> (be_>(30) ^^ ((_: String).toInt)))}
 ${"{'name' : 5.0}" must /("name" -> 5.0)}
 ${"{'name' : 5}" must /("name" -> 5.0)}
 ${"{'name' : 5.0}" must /("name" -> 5)}
 ${"{'name' : 5}" must /("name" -> 5)}
 ${"{'name' : 'Joe'}" must not(/("name2" -> "Joe"))}
 ${("['name', 'Joe']" must /("name" -> "initial")) returns "the array\n[name, Joe]\ndoesn't contain " +
      "the pair 'name':value 'initial'"}
 ${"{'name' : 'Joe'}" must /("n.*".r -> "j.*".r) returns "the object\n{name:Joe}\ndoesn't contain the pair 'n" +
      ".*':regex 'j" +
      ".*'"}
 ${("garbage" must /("name" -> "Joe")) returns "Could not parse\ngarbage"}
 with regexes as well
 ${"{'name' : 'Joe'}" must /("n.*".r -> "J.*".r)}

 The / matcher matches a value if the input is an Array
 ${"['name', 'Joe' ]" must /("name")}
 ${"[1.0, 2.0]" must /(1.0)}
 ${"{'name' : 'Joe'}" must not(/("name"))}
 ${("{'name' : 'Joe'}" must /("name")) returns "the object\n{name:Joe}\ndoesn't contain the value 'name'" +
      "\nThis selector can only be used with an array. Use /(k -> anyValue) if you just want to find the key 'k'"}
 ${("garbage" must /("name")) returns "Could not parse\ngarbage"}
 with regexes as well
 ${"['name', 'Joe']" must /("J.*".r)}
 ${"['name', 'Joe']" must /("j.*".r) returns "the array\n[name, Joe]\ndoesn't contain the regex 'j.*'"}

 The / matcher can be chained with another /
 ${"{'person' : {'name': 'Joe'}}" must /("person") / ("name" -> "Joe")}
 ${"{'person' : ['name']}" must /("person") / ("name")}
 ${"{'person' : { 'name': 'Joe', 'address' : {'street': 'here' }}}" must
      /("person") / ("address") / ("street" -> "here")}
 ${"{'person' : {'name': 'Joe', 'address': ['street']}}" must
      /("person") / ("address") / ("street")}

 The */ matcher matches a name and a value inside any Map
 ${"{'name' : 'Joe' }" must */("name" -> "Joe")}
 ${"{'person' : {'name': 'Joe'}}" must */("name" -> "Joe")}
 ${"{'person' : {'name': 'Joe'}}" must */("person" -> ".*".r)}
 ${"{'person' : {'name': 'Joe'}}" must */("name" -> ".*".r)}
 ${("{'person' : ['name', 'Joe']}" must not(*/("name" -> "Joe")))}
 ${("{'person' : ['name', 'Joe']}" must */("name" -> "Joe")) returns
      s"""the object\n{person:["name", "Joe"]}\ndoesn't contain the pair 'name':value 'Joe'"""}
 ${("garbage" must */("name" -> "Joe")) returns "Could not parse\ngarbage"}

 The */ matcher can be chained with /
 ${"{'person' : {'name': 'Joe'}}" must */("name") / ("Joe")}
 ${"{'person' : {'name': 'Joe', 'name': 'Moe'}}" must */("name") / ("Moe")}

 The */ matcher can be chained with */
 ${"{'person': {'address' : {'street' : 'here' }}}" must */("person") */ ("address") / ("street" -> "here")}

 The / matcher can be chained with */
 ${"{'person' : {'address' : {'street' : 'here'}}}" must /("person") */ ("street") / ("here")}

 The /#(i) matcher matches the i-th element in an Array
 ${"['name', 'Joe']" must /#(1) / ("Joe")}
 ${"['name', 'Joe']" must not(/#(1) / ("M.*.r"))}
 ${"{'person' : ['name', 'Joe'] }" must /("person") /# (1) / ("Joe")}
 ${"{'person' : ['name', ['Joe', 'Moe']] }" must /("person") /# (1) /# (1) / ("Moe")}
 ${"{'house' : {'person' : ['name', 'Joe']}}" must */("person") /# (1) / ("Joe")}

 The /#(i) matcher matches the i-th element in a Map
 ${"{'name' : 'Joe', 'name2' : 'Moe'}" must /#(1) / ("name2" -> "Moe")}
 ${"{'person' : {'name': 'Joe', 'name2' : 'Moe'} }" must /("person") /# (1) / ("name2" -> "Moe")}
 ${"{'house' : {'person' : {'name': 'Joe', 'name2' : 'Moe'}}}" must */("person") /# (1) / ("name2" -> "Moe")}

 have / andHave can be used to check the elements in an array
 ${"['name', 'Joe']" must have(size(2))}
 ${"[{'name': 'Joe'}]" must /#(0).andHave(size(1))}
 ${"{'name' : ['Joe']}" must /("name").andHave(size(1))}
 ${"{'person' : [{'names':['e', 't']}]}" must /("person")./#(0)./("names").andHave(size(2))}
 ${"{'names': ['e', 't']}" must /("names").andHave(size(2))}
 ${"{'person' : ['names', ['e', 't']] }" must /("person")./#(1).andHave(size(2))}

 have / andHave can be used to check the elements in an object
 ${"{'person' : {'name':'Joe', 'age':'18' } }" must /("person").andHave(exactly(/("name" -> "Joe"), /("age" -> "18")))}
 ${"{'person' : {'name':'Joe', 'age':'19' } }" must /("person").andHave(
      exactly(/("name" -> "Joe"), /("age" -> "18"))
    ) returns
      s"""the object\n{age:19}\ndoesn't contain the pair 'age':value '18'"""}

 String, Int, Boolean, Double and Traversable matchers can be used with the andHave method $andHave

 Matchers must be resilient when there are null values
 ${"""{ "b" : { "a" : 2, "c" : null } }""" must /("b" -> /("a" -> 2))}

"""

  def andHave =
    val json =
      """|{"products":[
         |{"name":"shirt","price":10,"visible":false,"collectionIds":["coll1"]},
         |{"name":"shoe","price":5,"visible":null},
         |{"name":"pants","price":5},
         |{"name":"tie","price":5},
         |{"name":"shoe","price":5,"visible":true}]}""".stripMargin

    def aProductWith(name: Matcher[JsonType], price: Matcher[JsonType], visible: Matcher[JsonType]): Matcher[String] =
      /("name").andHave(name) and
        /("price").andHave(price) and
        /("visible").andHave(visible)

    def haveProducts(products: Matcher[String]*): Matcher[String] =
      /("products").andHave(allOf(products*))

    json must haveProducts(
      aProductWith(name = "shirt", price = 10, visible = false) and /("collectionIds").andHave(exactly("coll1")),
      aProductWith(name = "shoe", price = 5, visible = beJsonNull),
      aProductWith(name = "shoe", price = 5, visible = true),
      /("name").andHave("pants") and /("price").andHave(5.0),
      /("name").andHave("tie") and /("price").andHave(BigDecimal(5.0))
    )

  // this example is taken from the liftweb project
  val person = """
{
  "person" : {
    "name" : "Joe",
    "age" : 35,
    "spouse" : {
      "person" : {
        "name" : "Marilyn",
        "age" : 33
      }
    }
  }
}
               """

}
