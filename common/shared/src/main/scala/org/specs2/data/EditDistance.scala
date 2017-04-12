package org.specs2
package data

/**
 * The EditDistance trait provides methods to compute the
 * distance between 2 sequences
 *
 * http://en.wikipedia.org/wiki/Edit_distance
 *
 */
trait EditDistance {

  /**
   * Edit matrix for 2 given sequences
   */
  class EditMatrix[T](s1: IndexedSeq[T], s2: IndexedSeq[T], costs: EditDistanceCosts[T]) {

    type DistanceMatrix = Array[Array[EditDistanceOp]]
    /* matrix containing the edit distance for any prefix of s1 and s2: matrix(i)(j) = edit distance(s1[0..i], s[0..j])*/
    private lazy val matrix = createDistanceMatrix(s1, s2)

    private def createDistanceMatrix(s1: IndexedSeq[T], s2: IndexedSeq[T]): DistanceMatrix = {
      val matrix = Array.ofDim[EditDistanceOp](s1.length + 1, s2.length + 1)

      for (i <- 0 to s1.length;
           j <- 0 to s2.length) {
        if (i == 0)      matrix(i)(j) = InsOp(j)                   // j insertions
        else if (j == 0) matrix(i)(j) = DelOp(i)                   // i suppressions
        else             matrix(i)(j) = cost(s1, s2, i, j, matrix) // otherwise
      }

      matrix
    }

    /** @return the cost for DistanceMatrix(i, j) */
    def cost(s1: IndexedSeq[T], s2: IndexedSeq[T], i: Int, j: Int, matrix: DistanceMatrix) = {
      val result = costs.lowerCost(s1(i - 1), s2(j - 1),
        matrix(i - 1)(j).cost     + costs.insertionDeletionCost(s1(i - 1)),       // suppression
        matrix(i - 1)(j - 1).cost + costs.substitutionCost(s1(i - 1), s2(j - 1)), // substitution
        matrix(i)(j - 1).cost     + costs.insertionDeletionCost(s2(j - 1)))       // insertion

      result match {
        case SubstOp(_) if matrix(i - 1)(j - 1).cost == result.cost => SameOp(result.cost)
        case _                                                      => result
      }
    }

    /** @return the edit distance between 2 strings */
    def distance = matrix(s1.length)(s2.length).cost

    /** prints the edit matrix of the 2 sequence */
    def showMatrix =
      matrix.map(_.mkString("|")).mkString("\n")

    /** show the differences between 2 sequences as a list of operations from one to the other */
    def operations: IndexedSeq[EditDistanceOperation[T]] = {
      def allOperations(i: Int, j: Int, operations: IndexedSeq[EditDistanceOperation[T]]): IndexedSeq[EditDistanceOperation[T]] = {
        if (i == 0 && j == 0) IndexedSeq()
        else {
          val op = matrix(i)(j)
          val dist = op.cost
          if (i == 1 && j == 1) {
            if (dist == 0) Same(s1(0)) +: operations
            else           Subst(s1(0), s2(0)) +: operations
          }
          else if (j < 1) s1.slice(0, i).map(Del.apply) ++ operations
          else if (i < 1) s2.slice(0, j).map(Add.apply) ++ operations
          else op match {
            case InsOp(_)   => allOperations(i,     j - 1, Add(s2(j - 1)) +: operations             )
            case DelOp(_)   => allOperations(i - 1, j,     Del(s1(i - 1)) +: operations             )
            case SubstOp(_) => allOperations(i - 1, j - 1, Subst(s1(i - 1), s2(j - 1)) +: operations)
            case _          => allOperations(i - 1, j - 1, Same(s1(i - 1)) +: operations            )
          }
        }
      }
      allOperations(s1.length, s2.length, IndexedSeq())
    }
  }

  def levenhsteinDistance[T : Equiv](s1: IndexedSeq[T], s2: IndexedSeq[T]): IndexedSeq[EditDistanceOperation[T]] = {
    val matrix = new EditMatrix[T](s1, s2, EditDistanceCosts.levenhsteinCosts[T])
    matrix.operations
  }

  trait EditDistanceOperation[T] {
    def t: T
    def inverse: EditDistanceOperation[T]
  }
  case class Add[T](t: T) extends EditDistanceOperation[T] {
    def inverse = Del(t)
  }
  case class Del[T](t: T) extends EditDistanceOperation[T] {
    def inverse = Add(t)
  }
  case class Same[T](t: T) extends EditDistanceOperation[T] {
    def inverse = Same(t)
  }
  case class Subst[T](t: T, t2: T) extends EditDistanceOperation[T] {
    def inverse = Subst(t2, t)
  }

}

object EditDistance extends EditDistance

trait EditDistanceOp {
  def cost: Int
}
case class InsOp(cost: Int) extends EditDistanceOp {
  override def toString = "+ "+cost
}
case class DelOp(cost: Int) extends EditDistanceOp {
  override def toString = "- "+cost
}
case class SubstOp(cost: Int) extends EditDistanceOp {
  override def toString = "~ "+cost
}
case class SameOp(cost: Int) extends EditDistanceOp {
  override def toString = "o "+cost
}

trait EditDistanceCosts[T] {
  /** @return the cost of a substitution */
  def substitutionCost(a: T, b: T): Int

  /** @return the cost of an insertion or deletion */
  def insertionDeletionCost(c: T): Int

  /** @return the lower cost and associated operation of a deletion, substitution or insertion */
  def lowerCost(a: T, b: T, del: Int, subst: Int, ins: Int): EditDistanceOp
}

object EditDistanceCosts {
  def levenhsteinCosts[T : Equiv]: EditDistanceCosts[T] =
    new LevenhsteinCosts[T] {
      val equiv = implicitly[Equiv[T]]
    }
}

trait LevenhsteinCosts[T] extends EditDistanceCosts[T] {
  def equiv: Equiv[T]

  /** @return the cost of a substitution */
  def substitutionCost(a: T, b: T): Int = if (equiv.equiv(a, b)) 0 else 1

  /** @return the cost of an insertion or deletion */
  def insertionDeletionCost(c: T) = 1

  /**
   * @return the lower cost and associated operation of a deletion, substitution or insertion
   *         in case of equality between a non-substitution and an insertion/suppression
   *         we select the insertion/suppression in order to group all the differences together
   *         diff("abcd", "acbd") ==> ("a[bc]d", "a[cb]d"). the distance is 2, otherwise
   *         diff("abcd", "acbd") ==> ("a[b]c[]d", "a[c]b[]d")
   */
  def lowerCost(a: T, b: T, del: Int, subst: Int, ins: Int): EditDistanceOp = {
    val (opDel, opSubst, opIns) = (DelOp(del), SubstOp(subst), InsOp(ins))
    if (ins < del) {
      if (ins < subst) opIns
      else if (ins == subst && equiv.equiv(a, b)) opIns
      else opSubst
    } else {
      if (del < subst) opDel
      else if (del == subst && equiv.equiv(a, b)) opDel
      else opSubst
    }
  }

}

object StringLevenhsteinCosts extends LevenhsteinCosts[Char] {
  val equiv: Equiv[Char] =
      Equiv.universal[Char]
}



