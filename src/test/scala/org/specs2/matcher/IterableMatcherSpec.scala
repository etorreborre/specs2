package org.specs2
package matcher

class IterableMatcherSpec extends SpecificationWithJUnit { def is = 
" 'contain' checks if one or several elements are present in an iterable"^
   "with one element"^
     "when present" ! 
      { (List(1, 2) must contain(1)).toResult.message must_== "'List(1, 2)' contains '1'" }^
     "when missing" ! 
      { (List(1, 2) must contain(0)).toResult.message must_== "'List(1, 2)' doesn't contain '0'" }^
   "with several elements"^
     "when present" ! 
      { (List(1, 2) must contain(1)).toResult.message must_== "'List(1, 2)' contains '1'" }^
     "when missing" ! 
      { (List(1, 2) must contain(0)).toResult.message must_== "'List(1, 2)' doesn't contain '0'" }^
p^
" 'containInOrder' checks if a list of elements is included in an iterable"^
   "when all elements are present in order" ! 
    { (List(1, 2, 3, 4) must containInOrder(2, 4)).toResult.message must_== 
       "'List(1, 2, 3, 4)' contains in order '2, 4'" }^
   "when one element is missing" ! 
    { (List(1, 2, 3, 4) must containInOrder(2, 5)).toResult.message must_== 
       "'List(1, 2, 3, 4)' doesn't contain in order '2, 5'" }^
   "when the order is wrong" ! 
    { (List(1, 2, 3, 4) must containInOrder(4, 2)).toResult.message must_== 
       "'List(1, 2, 3, 4)' doesn't contain in order '4, 2'" }^
end
}                                                                                          