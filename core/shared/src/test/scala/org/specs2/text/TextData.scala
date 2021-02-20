package org.specs2
package text

import org.scalacheck.*

trait TextData:

   given arbAsciiChar: Arbitrary[Char] = Arbitrary { Gen.choose(65, 122) }
   lazy val arbAsciiString: Arbitrary[String] = Arbitrary(Arbitrary.arbitrary[List[Char]] `map` (_.mkString))


object TextData extends TextData
