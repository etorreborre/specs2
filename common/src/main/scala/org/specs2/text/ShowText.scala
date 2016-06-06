package org.specs2.text


/*
  case class Show1[T1](show1: T1 => String = (_:T1).notNull) {
    def show2[T2](show2: T2 => String): Show2[T1, T2] =
      Show2(show1, show2)

    def show3[T2, T3](show3: T3 => String): Show3[T1, T2, T3] =
      Show3(show1, (_:T2).notNull, show3)
  }
  case class Show2[T1, T2](show1: T1 => String = (_:T1).notNull, show2: T2 => String = (_:T2).notNull)

  case class Show3[T1, T2, T3](show1: T1 => String = (_:T1).notNull, show2: T2 => String = (_:T2).notNull, show3: T3 => String = (_:T3).notNull)
 */

object ShowText {

  def main(args: Array[String]) = {
    println(generateAll(10))
  }

  def showCaseClasses(n: Int) = {
    (1 to n).map { i =>
      s"""|case class Show$i${types(i)}(${(1 to i).map(j => s"show$j: T$j => String = (_:T$j).notNull").mkString(",")}) {
          |  def show2[T2](show2: T2 => String): Show2[T1, T2] =
          |    Show2(show1, show2)
          |
          |  def show3[T2, T3](show3: T3 => String): Show3[T1, T2, T3] =
          |    Show3(show1, (_:T2).notNull, show3)
          |}
      """.stripMargin
      List("case class Table"+i+types(i)+"(override val titles: List[String], rows: List["+dataRow(i)+"], override val execute: Boolean = false) extends "+
        "Table(titles, execute) { outer =>",
        "  def |"+st(i)+"(row: "+dataRow(i, letter="S")+") = "+table(i)+"(titles, outer.rows :+ row, execute)",
        "  def |[R : AsResult](f: "+typesTuple(i)+" => R) = executeRow(f, execute)",
        "  def |>[R : AsResult](f: "+typesTuple(i)+" => R) = executeRow(f, true)",
        "  def executeRow[R : AsResult](f: "+typesTuple(i)+" => R, exec: Boolean): DecoratedResult[DataTable] = {",
        "    if (exec)",
        "      collect(rows map { (d: "+dataRow(i)+") => (d.showCells, AsResult(f("+(1 to i).map("d.t"+_).mkString(",")+")).execute) })",
        "    else DecoratedResult(DataTable(titles, Seq[DataTableRow]()), Success(\"ok\"))",
        "  }",
        "  def |@[M[_], R](f: "+typesTuple(i)+" => M[R])(implicit asResult: AsResult[R], app: Applicative[M]): M[DecoratedResult[DataTable]] =",
        "    executeRowApply(f, execute)",
        "  def |@>[M[_], R](f: "+typesTuple(i)+" => M[R])(implicit asResult: AsResult[R], app: Applicative[M]): M[DecoratedResult[DataTable]] =",
        "    executeRowApply(f, true)",
        "  def |*[R](f: "+typesTuple(i)+" => R)(implicit asResult: AsResult[R]): ExecutorService => DecoratedResult[DataTable] = (es: ExecutorService) => ",
        "    executeRowApply("+parametersList(i: Int)+" => Future.fork(Future.delay(f("+(1 to i).map("t"+_).mkString(",")+")))(es), execute)(asResult, control.FutureInstances.parallelApplicative).run",
        "  def |*>[R](f: "+typesTuple(i)+" => R)(implicit asResult: AsResult[R]): ExecutorService => DecoratedResult[DataTable] = (es: ExecutorService) => ",
        "    executeRowApply("+parametersList(i: Int)+" => Future.fork(Future.delay(f("+(1 to i).map("t"+_).mkString(",")+")))(es), true)(asResult, control.FutureInstances.parallelApplicative).run",
        "  def executeRowApply[R, M[_]](f: "+typesTuple(i)+" => M[R], exec: Boolean)(implicit asResult: AsResult[R], app: Applicative[M]): M[DecoratedResult[DataTable]] = {",
        "    if (exec)",
        "      app.map(app.traverse(rows.toList) { d: DataRow"+i+types(i)+" =>",
        "        app.map(f("+(1 to i).map("d.t"+_).mkString(",")+"))(r => (d.showCells, AsResult(r).execute))",
        "      })(rs => collect(rs))",
        "    else app.pure(DecoratedResult(DataTable(titles, Seq[DataTableRow]()), Success(\"ok\")))",
        "  }",
        "}").mkString("\n")
    }.mkString("\n")

  def types(i: Int) = typesAsList(i).mkString("[",", ", "]")
  def typesAsList(i: Int, letter: String = "T"): Seq[String] =  (1 to i).map(letter+_)

}
