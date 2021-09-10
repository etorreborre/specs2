package org.specs2
package guide

object RunInIDE extends UserGuidePage {
  def is = "Run in IDE".title ^ s2"""

### Intellij IDEA

[IntelliJ IDEA](https://github.com/jetbrains/intellij-scala) is the IDE with the best $specs2 integration for now. You can:

 * execute a specification by selecting its name and pressing `CTRL+SHIFT+F10`
 * execute a single example by selecting its description and pressing `CTRL+SHIFT+F10`

 ![specs2 in Intellij](${GUIDE_DIR}/images/intellij.png)

However passing arguments needs to be done through system properties for now. So if you need to use the `xonly` argument you need to pass `-Dspecs2.xonly`.

### VSCode

$specs2 is integrated to the [`metals` language server](https://scalameta.org/metals), which means that any compiling specification in VSCode should be
adorned with a small `test` button to run it.
"""
}
