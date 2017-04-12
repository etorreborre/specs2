package org.specs2
package guide

import specification.core.Fragments

object FragmentsApi extends UserGuidePage { def is = "Fragments API".title ^ s2"""
In $specs2 a specification can simply be viewed as a sequence of "Fragments". A `Fragment` is something which has:

 - a `Description`
 - an `Execution`

All the elements encountered in this User Guide fall under this representation:

 - an `Example` is a `Fragment` with a `Text` description and an `Execution` returning a `Result`
 - a `Step` is a `Fragment` with no description and a special `Execution` declaring that everything before the step must be executed before the `Step` is executed
 - a `Text` is a `Fragment` with a `Text` description and no `Execution`

The role of the various DSLs in acceptance and unit specifications is to create those fragments and assemble them into a bigger `Fragments` object. They do this using:

 - the `FragmentFactory` API to create individual fragments
 - the `Fragments` API to assemble them

### The `FragmentFactory` API

The `org.specs2.specification.create.FragmentFactory` trait possesses different methods to create:

 - texts
 - examples
 - steps / actions
 - tags
 - references
 - "formatting" fragments (break, paragraph, tab...)

Please have a look at the ScalaDoc to see the exact API for the factory and look at the source code for the default implementation in `org.specs2.specification.create.DefaultFragmentFactory`.

### The `Fragments` API

If you know how to create examples, texts and steps you will need to append them together as `Fragments`. You can create a `Fragments` object by using `Fragments.apply`:${snippet{
val ff = fragmentFactory

Fragments(ff.text("introduction"), ff.example("first example", success), ff.break)
}}


Then you can use the methods of the `org.specs2.specification.core.Fragments` class to add more fragments or to modify existing ones:

 Method                                          | Description
 ----------------------------------------------- | -----------
 `append(f: Fragment)`                           | to append a single fragment
 `append(fs: Fragments)`                         | to append another `Fragments` object
 `prepend(...)`                                  | to do the same as above but prepending instead of appending
 `append(fs: Seq[Fragment])`                     | to append a sequence
 `filter(f: Fragment => Boolean)`                | to filter out some fragments
 `map(f: Fragment => Fragment)`                  | to modify each fragment
 `mapDescription(f: Description => Description)` | to just modify the descriptions

### The `Fragments` DSL

The `org.specs2.specification.dsl.FragmentsDsl` trait provides a very versatile `^` operator to append fragments together, so you can write:${snippet{
val ff = fragmentFactory

val fs = Fragments(ff.text("introduction"), ff.example("first example", success), ff.break)
val f1 = ff.text("f1")

// all those combinations are possible and return a `Fragment` object
fs ^ f1
f1 ^ fs
fs ^ fs
f1 ^ f1

}}

"""
}

