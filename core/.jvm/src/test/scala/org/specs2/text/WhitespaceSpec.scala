package org.specs2
package text

import Whitespace.*

class WhitespaceSpec extends Specification:
  def is = s2"""

  ${ "Sentence with some   spaces".showSpaces === "Sentence⎵with⎵some⎵⎵⎵spaces" }
  ${ "Sentence\nwith\nsome\n\nnewlines".showNewlines === "Sentence↵with↵some↵↵newlines" }
  ${ "Sentence\twith\tsome\t\ttabs".showTabs === "Sentence→with→some→→tabs" }

  """
