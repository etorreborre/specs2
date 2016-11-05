package org.specs2.control
package eff

import async._

trait AsyncService {

  def asyncNow[R :_async, A](a: A): Eff[R, A]

  def asyncFail[R :_async](t: Throwable): Eff[R, Unit]

  def asyncDelay[R :_async, A](a: =>A): Eff[R, A]

  def asyncFork[R :_async, A](a: =>A): Eff[R, A]

  def fork[R :_async, A](a: =>Async[A]): Eff[R, A]

}


