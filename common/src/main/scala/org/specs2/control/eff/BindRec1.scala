package org.specs2.control.eff

import scalaz._

/** Papers over the parameter order change in Scalaz 7.3 */
trait BindRec1[M[_]] extends BindRec[M] {

  def tailrecM[A, B](a: A)(f: A => M[\/[A, B]]): M[B]

  // TODO: Needed for compatibility with Scalaz prior to 7.3
  def tailrecM[A, B](f: A => M[\/[A, B]])(a: A): M[B] =
    tailrecM(a)(f)

}
