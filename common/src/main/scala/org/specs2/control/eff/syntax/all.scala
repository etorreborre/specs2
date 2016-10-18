package org.specs2.control.eff.syntax

object all extends all

trait all extends
  disjunction with
  eval with
  error with
  writer with
  state with
  future with
  safe with
  console with
  warnings with
  async with
  eff

