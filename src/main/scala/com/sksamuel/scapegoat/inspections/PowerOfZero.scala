package com.sksamuel.scapegoat.inspections

/**
 * Created by Miwa on 2015/12/23.
 */

import com.sksamuel.scapegoat._

class PowerOfZero extends Inspection {

  def inspector(context: InspectionContext): Inspector = new Inspector(context) {
    override def postTyperTraverser = Some apply new context.Traverser {

      import context.global._

      private def isNumber(tree: Tree) = {
        tree.tpe <:< typeOf[Int] ||
          tree.tpe <:< typeOf[Long] ||
          tree.tpe <:< typeOf[Double] ||
          tree.tpe <:< typeOf[Float]
      }

      override def inspect(tree: Tree): Unit = {
        tree match {
          case Apply(Select(pack, TermName("pow")), List(Literal(Constant(0.0)), Select(lhs, TermName("toDouble")))) if pack.toString() == "java.this.lang.Math"  && isNumber(lhs)=>
            context.warn("Power of Zero",
              tree.pos, Levels.Warning, "Power of Zero always get number 0.0", PowerOfZero.this)
          case _ => continue(tree)
        }
      }
    }
  }
}

