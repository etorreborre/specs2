package org.specs2
package specification
package process

import scalaz.{Tree, TreeLoc}
import scalaz.syntax.foldable._
import scalaz.std.list._
import Tree._
import TreeLoc._
import data.Trees._
import scalaz.stream.{Process, Process1}
import specification.create._
import DefaultFragmentFactory._
import Process.{End =>_,_}
import specification.core._
import Fragment._
import scala.math._

trait Levels {

  def levelsProcess: Process1[Fragment, (Fragment, Int)] =
    levelsProcess1.map { case (f, level) => (f, level.l)}
  
  def levelsProcess1: Process1[Fragment, (Fragment, Level)] = {

    def go(level: Level): Process1[Fragment, (Fragment, Level)] = {

      def sameLevel(f: Fragment) = emit((f, level)) ++ go(level)
      def nextLevel(f: Fragment, next: Level) = emit((f, level)) ++ go(next)

      receive1 {
        // level goes +1 when a new block starts
        case f @ Fragment(Start,_ ,_) => nextLevel(f, level.copy(start = true, incrementNext = false))
        case f if Fragment.isText(f)    => nextLevel(f, level.copy(start = true, incrementNext = true))
        case f @ Fragment(End,_ ,_)   => nextLevel(f, level.copy(start = false, incrementNext = false, max(0, level.l - 1)))
        case f                          => 
          if (level.incrementNext) nextLevel(f, level.copy(start = false, incrementNext = false, l = level.l + 1))
          else                     sameLevel(f)
      }
    }
    go(Level())
  }
  
  case class Level(start: Boolean = false, incrementNext: Boolean = false, l: Int = 0)

  def levelsToTreeLoc(mapper: Mapper): Process1[(Fragment, Int), TreeLoc[Fragment]] = {

    def go(treeLoc: TreeLoc[(Fragment, Int)]): Process1[(Fragment, Int), TreeLoc[(Fragment, Int)]] = {
      receive1 {
        case (f, level) =>

          val parent = if (level == 0) treeLoc.root else (treeLoc.parentLocs :+ treeLoc).takeWhile(_.getLabel._2 < level).lastOption.getOrElse(treeLoc)
          val newTree = mapper(f) match {
            case Some(fragment) => parent.insertDownLast(leaf((fragment, level)))
            case None           => treeLoc
          }
          emit(newTree) ++ go(newTree)
      }
    }
    go(leaf((text("root"), 0)).loc).map(_.map(_._1))
  }

  def treeLoc(fs: Fragments): Option[TreeLoc[Fragment]] =
    treeLocMap(fs)(identityMapper)

  def levels(fs: Fragments): List[Int] =
     fs.contents.pipe(levelsProcess).runLog.run.map(_._2).toList


  def levels(f: Fragment): List[Int]   = levels(Fragments(f))
  def levels(structure: SpecStructure): List[Int] = levels(structure.fragments)

  def treeLocMap(fs: Fragments)(mapper: Mapper): Option[TreeLoc[Fragment]] =
    fs.contents.pipe(levelsProcess).pipe(levelsToTreeLoc(mapper)).runLog.run.lastOption

  def tree(fs: Fragments): Option[Tree[Fragment]] = treeLoc(fs).map(_.toTree)
  def treeMap(fs: Fragments)(mapper: Mapper): Option[Tree[Fragment]] = treeLocMap(fs)(mapper).map(_.toTree)
  def treeMap(structure: SpecStructure)(mapper: Mapper): Option[Tree[Fragment]] = treeMap(structure.fragments)(mapper)

  type Mapper = Fragment => Option[Fragment]
  val identityMapper: Mapper = (f: Fragment) => Some(f)
}

object Levels extends Levels