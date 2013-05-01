package org.specs2
package specification
package script

import execute.Snippet

trait Snippets extends Scripts with execute.Snippets { this: FragmentsBuilder =>
  case class SnippetsLibrary(title: String, snippets: Map[String, Snippet[_]] = Map(), isStart: Boolean = true)
                            (implicit template: ScriptTemplate[SnippetsLibrary, TextWithSnippets] = SnippetsDoc()) extends Script {

    def fragments(text: String): Fragments = {
      template.lines(text, this).lines.foldLeft(Fragments.createList()) { (res, cur) =>
        cur match {
          case NormalText(ls)  => res append Seq(Text(ls.mkString))
          case SnippetText(ls) => res append Seq(Text(ls.mkString))
        }
      }
    }

    def add(ref: String)(snippet: Snippet[_]) = copy(snippets = (snippets.+((ref, snippet))))

    def start = this
    def end = copy(isStart = false)

    def textFor(snippetRef: String) = snippets.get(snippetRef).map { snippet =>
      snippet.show() + (if (snippet.showResult.nonEmpty) "\n"+snippet.showResult else "")
    }.getOrElse(s"Snippet not found: $snippetRef")
  }

  case class SnippetsDoc(marker: String = ">(.+)<") extends ScriptTemplate[SnippetsLibrary, TextWithSnippets] {
    def lines(text: String, library: SnippetsLibrary): TextWithSnippets = {
      text.split("\n").foldLeft(TextWithSnippets(Seq())) { (res, cur) =>
        res.append {
          marker.r.findFirstMatchIn(cur.trim) match {
            case Some(snippetRef) => SnippetText(library.textFor(snippetRef.group(1).trim).trim+"\n")
            case None             => NormalText(cur+"\n")
          }
        }
      }
    }
  }

}

trait TextOrSnippet
case class NormalText(ls: Seq[String]) extends TextOrSnippet
object NormalText { def apply(l: String): NormalText = NormalText(Seq(l)) }
case class SnippetText(ls: Seq[String]) extends TextOrSnippet
object SnippetText { def apply(l: String): SnippetText = SnippetText(Seq(l)) }

case class TextWithSnippets(lines: Seq[TextOrSnippet] = Seq()) extends ScriptLines {
  def append(ts: TextOrSnippet) = (lines.lastOption, ts) match {
    case (Some(NormalText(ls1)), NormalText(ls2))   => copy(lines = lines.dropRight(1) :+ NormalText(ls1 ++ ls2))
    case (Some(SnippetText(ls1)), SnippetText(ls2)) => copy(lines = lines.dropRight(1) :+ SnippetText(ls1 ++ ls2))
    case other                                      => copy(lines = lines :+ ts)
  }
}

