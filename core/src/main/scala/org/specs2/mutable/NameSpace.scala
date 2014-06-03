package org.specs2
package mutable

import org.specs2.specification.Scope

/**
 * This marker trait can be used to scope variables which should only be used for a given set of fragments
 * @see MutableGivenWhenThenSpec
 */
trait NameSpace extends Scope

