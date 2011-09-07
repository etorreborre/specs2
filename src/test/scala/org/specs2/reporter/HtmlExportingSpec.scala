package org.specs2
package reporter


class HtmlExportingSpec extends Specification { def is =
                                                                                                                                            """
The HtmlExporting trait is responsible for exporting the executed specification:

  ExecutedSpecification => Unit

While the formal type of output is Unit, as for any export, it is expected that we have intermediate Html files written to disk:

  ExecutedSpecification => HtmlFiles => Unit


                                                                                                                                            """

}