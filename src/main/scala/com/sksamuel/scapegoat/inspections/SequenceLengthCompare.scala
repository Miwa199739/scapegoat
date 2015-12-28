package com.sksamuel.scapegoat.inspections

import com.sksamuel.scapegoat.{ Inspection, InspectionContext, Inspector, Levels }
/**
 * Created by Miwa on 2015/12/24.
 */

class SequenceLengthCompare extends Inspection {

  def inspector(context: InspectionContext): Inspector = new Inspector(context) {
    override def postTyperTraverser = Some apply new context.Traverser {

      import context.global._

      private def isSeq(tree: Tree) = {
        tree.tpe <:< typeOf[Seq[_]]
      }
      private def isNumber(tree: Tree) = {
        tree.tpe <:< typeOf[Int] ||
          tree.tpe <:< typeOf[Long] ||
          tree.tpe <:< typeOf[Double] ||
          tree.tpe <:< typeOf[Float]
      }
      //private val Append = TermName("$colon$plus")

      override def inspect(tree: Tree): Unit = {
        tree match {
          case Apply(Select(Select(lhs, TermName("length")), TermName("$greater")), List(lhv)) if isSeq(lhs) && isNumber(lhv) =>
            context.warn("Length calculation of Sequence might be “expensive” computation for some collection classes, ",
              tree.pos,
              Levels.Info,
              "Length calculation of Sequence might be “expensive” computation for some collection classes",
              SequenceLengthCompare.this)
          case _ => continue(tree)
        }
      }
    }
  }
}

