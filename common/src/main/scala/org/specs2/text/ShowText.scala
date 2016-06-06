package org.specs2
package text

import NotNullStrings._

case class Show1[T1](show1: T1 => String = (_:T1).notNull) {
  def show2[T2](show2: T2 => String): Show2[T1, T2] =
    Show2(show1, show2)

  def show3[T2, T3](show3: T3 => String): Show3[T1, T2, T3] =
    Show3(show1, (_:T2).notNull, show3)

  def show4[T2, T3, T4](show4: T4 => String): Show4[T1, T2, T3, T4] =
    Show4(show1, (_:T2).notNull, (_:T3).notNull, show4)

  def show5[T2, T3, T4, T5](show5: T5 => String): Show5[T1, T2, T3, T4, T5] =
    Show5(show1, (_:T2).notNull, (_:T3).notNull, (_:T4).notNull, show5)

  def show6[T2, T3, T4, T5, T6](show6: T6 => String): Show6[T1, T2, T3, T4, T5, T6] =
    Show6(show1, (_:T2).notNull, (_:T3).notNull, (_:T4).notNull, (_:T5).notNull, show6)

  def show7[T2, T3, T4, T5, T6, T7](show7: T7 => String): Show7[T1, T2, T3, T4, T5, T6, T7] =
    Show7(show1, (_:T2).notNull, (_:T3).notNull, (_:T4).notNull, (_:T5).notNull, (_:T6).notNull, show7)

  def show8[T2, T3, T4, T5, T6, T7, T8](show8: T8 => String): Show8[T1, T2, T3, T4, T5, T6, T7, T8] =
    Show8(show1, (_:T2).notNull, (_:T3).notNull, (_:T4).notNull, (_:T5).notNull, (_:T6).notNull, (_:T7).notNull, show8)

  def show9[T2, T3, T4, T5, T6, T7, T8, T9](show9: T9 => String): Show9[T1, T2, T3, T4, T5, T6, T7, T8, T9] =
    Show9(show1, (_:T2).notNull, (_:T3).notNull, (_:T4).notNull, (_:T5).notNull, (_:T6).notNull, (_:T7).notNull, (_:T8).notNull, show9)

  def show10[T2, T3, T4, T5, T6, T7, T8, T9, T10](show10: T10 => String): Show10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] =
    Show10(show1, (_:T2).notNull, (_:T3).notNull, (_:T4).notNull, (_:T5).notNull, (_:T6).notNull, (_:T7).notNull, (_:T8).notNull, (_:T9).notNull, show10)

  def show(t1: T1): (String) =
    (show1(t1))

  def showList(t1: T1): List[String] =
    List(show1(t1))

}

case class Show2[T1, T2](show1: T1 => String = (_:T1).notNull, show2: T2 => String = (_:T2).notNull) {
  def show3[T3](show3: T3 => String): Show3[T1, T2, T3] =
    Show3(show1, show2, show3)

  def show4[T3, T4](show4: T4 => String): Show4[T1, T2, T3, T4] =
    Show4(show1, show2, (_:T3).notNull, show4)

  def show5[T3, T4, T5](show5: T5 => String): Show5[T1, T2, T3, T4, T5] =
    Show5(show1, show2, (_:T3).notNull, (_:T4).notNull, show5)

  def show6[T3, T4, T5, T6](show6: T6 => String): Show6[T1, T2, T3, T4, T5, T6] =
    Show6(show1, show2, (_:T3).notNull, (_:T4).notNull, (_:T5).notNull, show6)

  def show7[T3, T4, T5, T6, T7](show7: T7 => String): Show7[T1, T2, T3, T4, T5, T6, T7] =
    Show7(show1, show2, (_:T3).notNull, (_:T4).notNull, (_:T5).notNull, (_:T6).notNull, show7)

  def show8[T3, T4, T5, T6, T7, T8](show8: T8 => String): Show8[T1, T2, T3, T4, T5, T6, T7, T8] =
    Show8(show1, show2, (_:T3).notNull, (_:T4).notNull, (_:T5).notNull, (_:T6).notNull, (_:T7).notNull, show8)

  def show9[T3, T4, T5, T6, T7, T8, T9](show9: T9 => String): Show9[T1, T2, T3, T4, T5, T6, T7, T8, T9] =
    Show9(show1, show2, (_:T3).notNull, (_:T4).notNull, (_:T5).notNull, (_:T6).notNull, (_:T7).notNull, (_:T8).notNull, show9)

  def show10[T3, T4, T5, T6, T7, T8, T9, T10](show10: T10 => String): Show10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] =
    Show10(show1, show2, (_:T3).notNull, (_:T4).notNull, (_:T5).notNull, (_:T6).notNull, (_:T7).notNull, (_:T8).notNull, (_:T9).notNull, show10)

  def show(t1: T1, t2: T2): (String, String) =
    (show1(t1), show2(t2))

  def showList(t1: T1, t2: T2): List[String] =
    List(show1(t1), show2(t2))

}

case class Show3[T1, T2, T3](show1: T1 => String = (_:T1).notNull, show2: T2 => String = (_:T2).notNull, show3: T3 => String = (_:T3).notNull) {
  def show4[T4](show4: T4 => String): Show4[T1, T2, T3, T4] =
    Show4(show1, show2, show3, show4)

  def show5[T4, T5](show5: T5 => String): Show5[T1, T2, T3, T4, T5] =
    Show5(show1, show2, show3, (_:T4).notNull, show5)

  def show6[T4, T5, T6](show6: T6 => String): Show6[T1, T2, T3, T4, T5, T6] =
    Show6(show1, show2, show3, (_:T4).notNull, (_:T5).notNull, show6)

  def show7[T4, T5, T6, T7](show7: T7 => String): Show7[T1, T2, T3, T4, T5, T6, T7] =
    Show7(show1, show2, show3, (_:T4).notNull, (_:T5).notNull, (_:T6).notNull, show7)

  def show8[T4, T5, T6, T7, T8](show8: T8 => String): Show8[T1, T2, T3, T4, T5, T6, T7, T8] =
    Show8(show1, show2, show3, (_:T4).notNull, (_:T5).notNull, (_:T6).notNull, (_:T7).notNull, show8)

  def show9[T4, T5, T6, T7, T8, T9](show9: T9 => String): Show9[T1, T2, T3, T4, T5, T6, T7, T8, T9] =
    Show9(show1, show2, show3, (_:T4).notNull, (_:T5).notNull, (_:T6).notNull, (_:T7).notNull, (_:T8).notNull, show9)

  def show10[T4, T5, T6, T7, T8, T9, T10](show10: T10 => String): Show10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] =
    Show10(show1, show2, show3, (_:T4).notNull, (_:T5).notNull, (_:T6).notNull, (_:T7).notNull, (_:T8).notNull, (_:T9).notNull, show10)

  def show(t1: T1, t2: T2, t3: T3): (String, String, String) =
    (show1(t1), show2(t2), show3(t3))

  def showList(t1: T1, t2: T2, t3: T3): List[String] =
    List(show1(t1), show2(t2), show3(t3))

}

case class Show4[T1, T2, T3, T4](show1: T1 => String = (_:T1).notNull, show2: T2 => String = (_:T2).notNull, show3: T3 => String = (_:T3).notNull, show4: T4 => String = (_:T4).notNull) {
  def show5[T5](show5: T5 => String): Show5[T1, T2, T3, T4, T5] =
    Show5(show1, show2, show3, show4, show5)

  def show6[T5, T6](show6: T6 => String): Show6[T1, T2, T3, T4, T5, T6] =
    Show6(show1, show2, show3, show4, (_:T5).notNull, show6)

  def show7[T5, T6, T7](show7: T7 => String): Show7[T1, T2, T3, T4, T5, T6, T7] =
    Show7(show1, show2, show3, show4, (_:T5).notNull, (_:T6).notNull, show7)

  def show8[T5, T6, T7, T8](show8: T8 => String): Show8[T1, T2, T3, T4, T5, T6, T7, T8] =
    Show8(show1, show2, show3, show4, (_:T5).notNull, (_:T6).notNull, (_:T7).notNull, show8)

  def show9[T5, T6, T7, T8, T9](show9: T9 => String): Show9[T1, T2, T3, T4, T5, T6, T7, T8, T9] =
    Show9(show1, show2, show3, show4, (_:T5).notNull, (_:T6).notNull, (_:T7).notNull, (_:T8).notNull, show9)

  def show10[T5, T6, T7, T8, T9, T10](show10: T10 => String): Show10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] =
    Show10(show1, show2, show3, show4, (_:T5).notNull, (_:T6).notNull, (_:T7).notNull, (_:T8).notNull, (_:T9).notNull, show10)

  def show(t1: T1, t2: T2, t3: T3, t4: T4): (String, String, String, String) =
    (show1(t1), show2(t2), show3(t3), show4(t4))

  def showList(t1: T1, t2: T2, t3: T3, t4: T4): List[String] =
    List(show1(t1), show2(t2), show3(t3), show4(t4))

}

case class Show5[T1, T2, T3, T4, T5](show1: T1 => String = (_:T1).notNull, show2: T2 => String = (_:T2).notNull, show3: T3 => String = (_:T3).notNull, show4: T4 => String = (_:T4).notNull, show5: T5 => String = (_:T5).notNull) {
  def show6[T6](show6: T6 => String): Show6[T1, T2, T3, T4, T5, T6] =
    Show6(show1, show2, show3, show4, show5, show6)

  def show7[T6, T7](show7: T7 => String): Show7[T1, T2, T3, T4, T5, T6, T7] =
    Show7(show1, show2, show3, show4, show5, (_:T6).notNull, show7)

  def show8[T6, T7, T8](show8: T8 => String): Show8[T1, T2, T3, T4, T5, T6, T7, T8] =
    Show8(show1, show2, show3, show4, show5, (_:T6).notNull, (_:T7).notNull, show8)

  def show9[T6, T7, T8, T9](show9: T9 => String): Show9[T1, T2, T3, T4, T5, T6, T7, T8, T9] =
    Show9(show1, show2, show3, show4, show5, (_:T6).notNull, (_:T7).notNull, (_:T8).notNull, show9)

  def show10[T6, T7, T8, T9, T10](show10: T10 => String): Show10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] =
    Show10(show1, show2, show3, show4, show5, (_:T6).notNull, (_:T7).notNull, (_:T8).notNull, (_:T9).notNull, show10)

  def show(t1: T1, t2: T2, t3: T3, t4: T4, t5: T5): (String, String, String, String, String) =
    (show1(t1), show2(t2), show3(t3), show4(t4), show5(t5))

  def showList(t1: T1, t2: T2, t3: T3, t4: T4, t5: T5): List[String] =
    List(show1(t1), show2(t2), show3(t3), show4(t4), show5(t5))

}

case class Show6[T1, T2, T3, T4, T5, T6](show1: T1 => String = (_:T1).notNull, show2: T2 => String = (_:T2).notNull, show3: T3 => String = (_:T3).notNull, show4: T4 => String = (_:T4).notNull, show5: T5 => String = (_:T5).notNull, show6: T6 => String = (_:T6).notNull) {
  def show7[T7](show7: T7 => String): Show7[T1, T2, T3, T4, T5, T6, T7] =
    Show7(show1, show2, show3, show4, show5, show6, show7)

  def show8[T7, T8](show8: T8 => String): Show8[T1, T2, T3, T4, T5, T6, T7, T8] =
    Show8(show1, show2, show3, show4, show5, show6, (_:T7).notNull, show8)

  def show9[T7, T8, T9](show9: T9 => String): Show9[T1, T2, T3, T4, T5, T6, T7, T8, T9] =
    Show9(show1, show2, show3, show4, show5, show6, (_:T7).notNull, (_:T8).notNull, show9)

  def show10[T7, T8, T9, T10](show10: T10 => String): Show10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] =
    Show10(show1, show2, show3, show4, show5, show6, (_:T7).notNull, (_:T8).notNull, (_:T9).notNull, show10)

  def show(t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6): (String, String, String, String, String, String) =
    (show1(t1), show2(t2), show3(t3), show4(t4), show5(t5), show6(t6))

  def showList(t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6): List[String] =
    List(show1(t1), show2(t2), show3(t3), show4(t4), show5(t5), show6(t6))

}

case class Show7[T1, T2, T3, T4, T5, T6, T7](show1: T1 => String = (_:T1).notNull, show2: T2 => String = (_:T2).notNull, show3: T3 => String = (_:T3).notNull, show4: T4 => String = (_:T4).notNull, show5: T5 => String = (_:T5).notNull, show6: T6 => String = (_:T6).notNull, show7: T7 => String = (_:T7).notNull) {
  def show8[T8](show8: T8 => String): Show8[T1, T2, T3, T4, T5, T6, T7, T8] =
    Show8(show1, show2, show3, show4, show5, show6, show7, show8)

  def show9[T8, T9](show9: T9 => String): Show9[T1, T2, T3, T4, T5, T6, T7, T8, T9] =
    Show9(show1, show2, show3, show4, show5, show6, show7, (_:T8).notNull, show9)

  def show10[T8, T9, T10](show10: T10 => String): Show10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] =
    Show10(show1, show2, show3, show4, show5, show6, show7, (_:T8).notNull, (_:T9).notNull, show10)

  def show(t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7): (String, String, String, String, String, String, String) =
    (show1(t1), show2(t2), show3(t3), show4(t4), show5(t5), show6(t6), show7(t7))

  def showList(t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7): List[String] =
    List(show1(t1), show2(t2), show3(t3), show4(t4), show5(t5), show6(t6), show7(t7))

}

case class Show8[T1, T2, T3, T4, T5, T6, T7, T8](show1: T1 => String = (_:T1).notNull, show2: T2 => String = (_:T2).notNull, show3: T3 => String = (_:T3).notNull, show4: T4 => String = (_:T4).notNull, show5: T5 => String = (_:T5).notNull, show6: T6 => String = (_:T6).notNull, show7: T7 => String = (_:T7).notNull, show8: T8 => String = (_:T8).notNull) {
  def show9[T9](show9: T9 => String): Show9[T1, T2, T3, T4, T5, T6, T7, T8, T9] =
    Show9(show1, show2, show3, show4, show5, show6, show7, show8, show9)

  def show10[T9, T10](show10: T10 => String): Show10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] =
    Show10(show1, show2, show3, show4, show5, show6, show7, show8, (_:T9).notNull, show10)

  def show(t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8): (String, String, String, String, String, String, String, String) =
    (show1(t1), show2(t2), show3(t3), show4(t4), show5(t5), show6(t6), show7(t7), show8(t8))

  def showList(t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8): List[String] =
    List(show1(t1), show2(t2), show3(t3), show4(t4), show5(t5), show6(t6), show7(t7), show8(t8))

}

case class Show9[T1, T2, T3, T4, T5, T6, T7, T8, T9](show1: T1 => String = (_:T1).notNull, show2: T2 => String = (_:T2).notNull, show3: T3 => String = (_:T3).notNull, show4: T4 => String = (_:T4).notNull, show5: T5 => String = (_:T5).notNull, show6: T6 => String = (_:T6).notNull, show7: T7 => String = (_:T7).notNull, show8: T8 => String = (_:T8).notNull, show9: T9 => String = (_:T9).notNull) {
  def show10[T10](show10: T10 => String): Show10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] =
    Show10(show1, show2, show3, show4, show5, show6, show7, show8, show9, show10)

  def show(t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9): (String, String, String, String, String, String, String, String, String) =
    (show1(t1), show2(t2), show3(t3), show4(t4), show5(t5), show6(t6), show7(t7), show8(t8), show9(t9))

  def showList(t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9): List[String] =
    List(show1(t1), show2(t2), show3(t3), show4(t4), show5(t5), show6(t6), show7(t7), show8(t8), show9(t9))

}

case class Show10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](show1: T1 => String = (_:T1).notNull, show2: T2 => String = (_:T2).notNull, show3: T3 => String = (_:T3).notNull, show4: T4 => String = (_:T4).notNull, show5: T5 => String = (_:T5).notNull, show6: T6 => String = (_:T6).notNull, show7: T7 => String = (_:T7).notNull, show8: T8 => String = (_:T8).notNull, show9: T9 => String = (_:T9).notNull, show10: T10 => String = (_:T10).notNull) {

  def show(t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9, t10: T10): (String, String, String, String, String, String, String, String, String, String) =
    (show1(t1), show2(t2), show3(t3), show4(t4), show5(t5), show6(t6), show7(t7), show8(t8), show9(t9), show10(t10))

  def showList(t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9, t10: T10): List[String] =
    List(show1(t1), show2(t2), show3(t3), show4(t4), show5(t5), show6(t6), show7(t7), show8(t8), show9(t9), show10(t10))

}


object ShowText {
  def show1[T1](show1: T1 => String): Show1[T1] =
    Show1[T1](show1)
}

object ShowTextGenerator {

  def main(args: Array[String]) =
    println(generateAll(10))

  def generateAll(n: Int): String =
    showCaseClasses(n)

  def showCaseClasses(n: Int) =
    (1 to n).map { i =>
      s"""|case class Show$i${types(i)}(${(1 to i).map(j => s"show$j: T$j => String = (_:T$j).notNull").mkString(", ")}) {
          |  ${showMethods(i, n)}
          |  ${showMethod(i)}
          |  ${showListMethod(i)}
          |}
      """.stripMargin
    }.mkString("\n")

  def showMethods(i: Int, n: Int): String =
    ((i+1) to n).map { j =>
      s"""|  def show$j[${((i+1) to j).map(k => s"T$k").mkString(", ")}](show$j: T$j => String): Show$j${types(j)} =
          |    Show$j(${((1 to i).map(k => s"show$k") ++ ((i + 1) until j).map(k => s"(_:T$k).notNull") ++ Seq(s"show$j")).mkString(", ")})
          |""".stripMargin
    }.mkString("\n")

  def showMethod(i: Int): String =
    s"""|  def show(${(1 to i).map(j => s"t$j: T$j").mkString(", ")}): (${(1 to i).map(_ => "String").mkString(", ")}) =
        |    (${(1 to i).map(j => s"show$j(t$j)").mkString(", ")})
        |""".stripMargin

  def showListMethod(i: Int): String =
    s"""|  def showList(${(1 to i).map(j => s"t$j: T$j").mkString(", ")}): List[String] =
        |    List(${(1 to i).map(j => s"show$j(t$j)").mkString(", ")})
        |""".stripMargin

  def types(i: Int): String =
    typesAsList(i).mkString("[",", ", "]")

  def typesAsList(i: Int, letter: String = "T"): Seq[String] =
    (1 to i).map(letter+_)

}

