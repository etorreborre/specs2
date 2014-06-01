package org.specs2
package specification

import matcher.{StandardMatchResults, ShouldMatchers, MustMatchers}
import execute.{PendingUntilFixed, StandardResults}
import control.Debug
import control.ImplicitParameters


trait SpecificationFeatures extends
MustMatchers
with ShouldMatchers
with StandardResults
with StandardMatchResults
with PendingUntilFixed
with ImplicitParameters
with Debug

