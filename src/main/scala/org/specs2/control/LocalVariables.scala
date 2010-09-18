package org.specs2
package control

trait LocalVariables {
  /**
   * save the value of a variable, set it temporarily to another value and reset it when f is executed
   */
  def setTemporarily[T, U](current: T, temp: T, setter: T => Unit)(f: =>U) = {
    var saved = current
    try {
      setter(temp)
      f
    } finally {
      setter(saved)
    }
  }
  /**
   * save the value of 2 variables, set them temporarily to other values and reset them when f is executed
   */
  def setTemporarily[S, T, U](current1: S, temp1: S, setter1: S => Unit, 
                              current2: T, temp2: T, setter2: T => Unit)(f: =>U): U = {
    setTemporarily(current1, temp1, setter1) {
      setTemporarily(current2, temp2, setter2) {
        f
      }      
    }
  }
  /**
   * save the value of 3 variables, set them temporarily to other values and reset them when f is executed
   */
  def setTemporarily[R, S, T, U](current1: R, temp1: R, setter1: R => Unit, 
                                 current2: S, temp2: S, setter2: S => Unit,
                                 current3: T, temp3: T, setter3: T => Unit)(f: =>U): U = {
    setTemporarily(current1, temp1, setter1) {
      setTemporarily(current2, temp2, setter2) {
        setTemporarily(current3, temp3, setter3) {
          f
        }
      }      
    }
  }
}
object LocalVariables extends LocalVariables