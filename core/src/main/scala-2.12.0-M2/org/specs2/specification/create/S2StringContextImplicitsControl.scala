package org.specs2
package specification
package create

import main.CommandLineAsResult
import specification.core.Env

trait S2StringContextImplicitsControl extends S2StringContext1 { this: S2StringContext =>
  implicit def commandLineAsResultIsInterpolatedFragment[R : CommandLineAsResult](r: =>R): InterpolatedFragment =
    envFunctionIsInterpolatedFragment((env: Env) => implicitly[CommandLineAsResult[R]].asResult(env.arguments.commandLine, r))
}
