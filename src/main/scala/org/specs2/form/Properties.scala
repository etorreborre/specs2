package org.specs2
package form

private[specs2]
trait Properties {
  /**
   * This method is used setup a property value, in order to avoid repeating a string. For example: <pre>
   * The name of the person should be {"john" as personName in checkPersonName}
   * </pre>. 
   */
  implicit def anyToAs[T](a: T) = new AsProperty(a)
  implicit def propertyToValue[T](p: Property[T]):T = p()
  case class AsProperty[T](a: T) { 
    def as(p: Property[T]) = {p() = a; a }
    def apply(p: Property[T]) = {p() = a; a}
    def apply(f: T => Any)= {f(a); a }
    def as(f: T => Any)= {f(a); a }
  }
}
private[specs2]
object Properties extends Properties
