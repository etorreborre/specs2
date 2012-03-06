package org.specs2
package reflect

private[specs2]
object Generation {
  def typeParameters(n: Int) = (1 to n).map(j => "T"+j).mkString("[", ",", ",R]")
  def parametersAndTypes(n: Int) = (1 to n).map(j => "t"+j+":T"+j).mkString(",")
  def tupleTypes(n: Int) = (1 to n).map(j => "T"+j).mkString("(", ",", ")")
  def parameters(n: Int) = (1 to n).map(j => "t"+j).mkString(",")
  def function(n: Int) = "Function"+n+(1 to n).map(j => "T"+j).mkString("[", ",", ",R]")
  def values(n: Int) = (1 to n).map(j => "values._1._"+j).mkString("(", ",", ", values._2)")
}
