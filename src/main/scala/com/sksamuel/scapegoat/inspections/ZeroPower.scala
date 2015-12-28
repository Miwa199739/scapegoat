package com.sksamuel.scapegoat.inspections

import com.sksamuel.scapegoat._

/**
 * Created by Miwa on 2015/12/21.
 */

class ZeroPower extends Inspection {

  def inspector(context: InspectionContext): Inspector = new Inspector(context) {
    override def postTyperTraverser = Some apply new context.Traverser {

      import context.global._

      override def inspect(tree: Tree): Unit = {
        tree match {
          case Apply(Select(pack, TermName("pow")), List(Select(Ident(TermName(_)), TermName("toDouble")), Literal(Constant(0.0)))) if pack.toString() == "java.this.lang.Math" =>
            context.warn("Zero Power",
              tree.pos, Levels.Warning, "Zero Power always get number 1.0", ZeroPower.this)
          case _ => continue(tree)
        }
      }
    }
  }
}