package org.specs2
package specification
package core

import org.scalacheck.util.Pretty
import org.scalacheck.util.Pretty.Params
import org.specs2.fp.syntax._

object Pretties {

  def prettyFragments: Fragments => Pretty = (fs: Fragments) => Pretty { params: Params =>
    fs.fragments.map(f => prettyFragment(f).apply(params)).mkString("Fragments(\n", "\n", ")\n")
  }

  def prettyFragment: Fragment => Pretty = (f: Fragment) => Pretty { params: Params =>
    f.show
  }
}
