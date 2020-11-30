package user

import org.specs2._

class TestSpec extends Specification with ScalaCheck { def is = s2"""

 e1 $e1

"""

  def e1 = prop { (n: Int) =>
    n ==== n
  }
}

class TestMutableSpec extends org.specs2.mutable.Specification:

  "e1" >> ok

trait Blocks:
  trait ToBlock[S, R]:
    def toBlock(d: String, s: S): R

  given ToBlock[F, F]:
    def toBlock(s: String, f: F): F =  f

  given ToBlock[FS, FS]:
    def toBlock(s: String, fs: FS): FS = fs

  given [R : AsE] as ToBlock[R, F]:
    def toBlock(s: String, r: R): F =
      new F {}

  extension [S,R] (d: String):
    def >>>(s: S)(using t: ToBlock[S, R]): R =
     summon[ToBlock[S, R]].toBlock(d, s)

trait S extends Blocks

object Test:
  new S:
    "try this" >>> {
      "in that" >>> new R {}
    }


trait F
trait FS
trait E
trait R

trait AsE[R]:
 def toE(r: =>R): E

given AsE[R]:
 def toE(r: =>R) = new E {}
