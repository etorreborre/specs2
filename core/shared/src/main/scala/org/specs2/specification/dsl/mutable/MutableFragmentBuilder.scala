package org.specs2
package specification
package dsl
package mutable

import main.Arguments
import specification.create.FragmentsFactory
import specification.core._

/**
 * Creation of fragments in a mutable specification
 *
 * This essentially works by keep a mutable ListBuffer of Fragments
 *
 * Arguments and title are also added with mutable variables
 *
 *
 */
trait MutableFragmentBuilder extends FragmentBuilder
  with FragmentsFactory
  with MutableArgumentsBuilder
  with MutableHeaderBuilder { outer =>

  private[specs2] var specFragments = Fragments.empty

  // if this variable is true then each block will have section markers
  private[specs2] var addSectionsForBlocks = false

  def specificationFragments =
    // add 2 line breaks just after the specification title
    specFragments.prepend(List.fill(2)(fragmentFactory.break))

  def is: SpecStructure =
    SpecStructure.create(headerVar, argumentsVar, specificationFragments)

  def addFragmentsBlock(fs: =>Fragments): Fragments =
    fs
    addFragment(fragmentFactory.end)
    Fragments.empty

  def addFragment(fragment: Fragment): Fragment =
    specFragments = specFragments.append(fragment)
    fragment

  def addFragments(fragments: Fragments): Fragments =
    specFragments = specFragments.append(fragments)
    fragments

  def addSections(): Unit =
    addSectionsForBlocks = true

  def hasSectionsForBlocks: Boolean =
    addSectionsForBlocks

}

trait FragmentBuilder:
  def addSections(): Unit
  def hasSectionsForBlocks: Boolean
  def addFragment(f: Fragment): Fragment
  def addFragments(fs: Fragments): Fragments
  def addFragmentsBlock(block: =>Fragments): Fragments

trait MutableHeaderBuilder:
  private[specs2] var headerVar = new SpecHeader(specClass = getClass)
  def setTitle(t: String) =
    headerVar = headerVar.copy(title = Some(t))
    headerVar

trait MutableArgumentsBuilder:
  private[specs2] var argumentsVar = Arguments()
  def updateArguments(a: Arguments): Arguments = { argumentsVar = argumentsVar <| a; a }
  def setArguments(a: Arguments): Arguments = { argumentsVar = a; a }
