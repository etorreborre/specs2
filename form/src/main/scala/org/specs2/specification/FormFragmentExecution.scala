package org.specs2
package specification

import main.Arguments
import time.SimpleTimer

/**
 * This trait executes Form Fragments in addition to other fragments
 */
trait FormFragmentExecution extends FragmentExecution {

  /**
   * execute a Fragment.
   *
   * A Form is executed separately by executing each row and cell, setting the results on each cell
   */
  override def execute(f: Fragment)(implicit arguments: Arguments = Arguments()) = f match {
    case e @ Example(ff: FormFormattedString, _,_,_,_)     => {
      val timer = new SimpleTimer().start
      val executed = if (arguments.plan) ff.form else ff.form.executeForm
      val result = executed.execute
      ExecutedResult(FormFormattedString.create(executed), result, timer.stop, f.location, Stats(result).copy(timer = timer.stop))
    }
    case other => super.execute(f)(arguments)
  }
}

