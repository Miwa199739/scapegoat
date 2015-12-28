package com.sksamuel.scapegoat.inspections

import com.sksamuel.scapegoat.{ Inspection, InspectionContext, Inspector, Levels }
/**
 * Created by Miwa on 2015/12/22.
 */

class SimplifyExpression extends Inspection {

  def inspector(context: InspectionContext): Inspector = new Inspector(context) {
    override def postTyperTraverser = Some apply new context.Traverser {

      import context.global._

      private val Equals = TermName("$eq$eq")

      override def inspect(tree: Tree): Unit = {
        tree match {
          case Apply(Select(lhs, Equals), List(Literal(Constant(true)))) =>
            context.warn("Simplify expressions",
              tree.pos,
              Levels.Info,
              "Boolean expressions x == true can be re-written as x",
              SimplifyExpression.this)
          case _ => continue(tree)
        }
      }

    }
  }
}