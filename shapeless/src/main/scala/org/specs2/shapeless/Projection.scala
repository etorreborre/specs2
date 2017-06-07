package org.specs2.shapeless

import shapeless._
import org.specs2.control._

/**
 * implicit instances for projecting objects of a type A on objects of a type B
 */
trait Projection {

  def apply[A, B](prj: A => B): A ProjectsOn B =
    (a: A) => prj(a)

  // This allows us to obtain an implicit evidence parameter when A cannot be projected onto B.
  implicit def notProjectsOn[A, B] = new DoesNotProjectOn[A, B] {}
  implicit def notProjectsOn1[A, B](implicit ev: A ProjectsOn B): DoesNotProjectOn[A, B] = { Use(ev); unexpected }
  implicit def notProjectsOn2[A, B](implicit ev: A ProjectsOn B): DoesNotProjectOn[A, B] = { Use(ev); unexpected }

  // Each type can be projected onto itself.
  implicit def selfProjection[A, B](implicit ev: A =:= B): A ProjectsOn B =
    Projection(a => a)

  // Each HList can be projected onto HNil.
  implicit def hnilProjection[H <: HList]: H ProjectsOn HNil =
    Projection(_ => HNil)

  // An HList can be projected onto another one if its tail can be projected.
  implicit def tailProjection[X, XS <: HList, Y, YS <: HList](implicit
                                                              pr: XS ProjectsOn (Y :: YS),
                                                              ne: X  DoesNotProjectOn Y
                                                             ): (X :: XS) ProjectsOn (Y :: YS) = {
    Use(ne)
    Projection(xxs => pr(xxs.tail))
  }

  // An HList can be projected onto another one if its head and tail can be projected.
  implicit def hlistProjection[X, XS <: HList, Y, YS <: HList](implicit
                                                               xy: Lazy[X ProjectsOn Y],
                                                               rs: XS ProjectsOn YS
                                                              ): (X :: XS) ProjectsOn (Y :: YS) =
    Projection(xxs => xy.value.apply(xxs.head) :: rs(xxs.tail))

  // A case class can be projected onto another one if the types are not subtypes of AnyVal and if
  // there exists a projection between their generic representations.
  implicit def reprProjection[A, B, ARepr <: HList, BRepr <: HList](implicit
                                                                    ar: Generic.Aux[A, ARepr],
                                                                    br: Generic.Aux[B, BRepr],
                                                                    pr: Lazy[ARepr ProjectsOn BRepr],
                                                                    ne: A =:!= B,
                                                                    aa: A <:!< AnyVal,
                                                                    ba: B <:!< AnyVal
                                                                   ): A ProjectsOn B = {
    Use(aa, ba, ne)
    Projection(a => br.from(pr.value.apply(ar.to(a))))
  }

  implicit class ProjectionOps[A](a: A) {
    def projectOn[B](implicit projection: ProjectsOn[A, B]): B =
      projection(a)
  }

}

object Projection extends Projection

/**
 * type class for projecting an object of type to another type B
 */
trait ProjectsOn[A, B] {
  def apply(a: A): B
}

/**
 * type class for a type A which can *not* be projected on a type B
 */
trait DoesNotProjectOn[A, B]
