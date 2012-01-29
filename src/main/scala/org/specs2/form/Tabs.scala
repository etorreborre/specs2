package org.specs2
package form

import scala.xml._
import xml.Nodex._
import main._
import execute._
import StandardResults._

/**
 * This class allows the creation of tabs to embed several forms at once on a limited html space
 * @see org.specs2.examples.FormSpec
 */
case class Tabs(tabs: Seq[Tab] = Vector(), result: Option[Result] = None) extends Cell {
  def tab(t: String, form: Form) = Tabs(tabs :+ Tab(t, form))
  def tabs(ts: Tabs): Tabs = Tabs(tabs ++ ts.tabs)

  def setSuccess = copy(result = Some(success))
  def setFailure = copy(result = Some(failure))
  def execute = result.getOrElse(executeTabs)
  def executeCell = copy(result = result.orElse(Some(executeTabs)))

  def text: String = tabs.map(_.text).mkString("\n")

  def xml(implicit args: Arguments) = <td class="info"><div class="tabber">{tabs.map(_.xml).reduceNodes}</div></td>

  def executeTabs = tabs.foldLeft(success: Result){ (res, cur) => res and cur.execute }
}

/**
 * Class representing an individual tab
 */
case class Tab(title: String, form: Form, result: Option[Result] = None) extends Cell {
  def setSuccess = copy(result = Some(success))
  def setFailure = copy(result = Some(failure))

  def execute = result.getOrElse(form.execute)
  def executeCell = copy(result = result.orElse(Some(form.execute)))

  def text: String = title + "\n" + new FormCell(form).text

  def xml(implicit args: Arguments) = <div class="tabbertab" title={title}>{Form.toXml(form.executeForm)}</div>

}