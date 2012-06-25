/**
 * User: slesarenko
 * Date: 6/25/12
 */
package main.scala.scalan.staged

import scala.virtualization.lms.internal.Transforming
import scalan.dsl.ArraysBase

trait Fusion extends Transforming { self: ArraysBase =>

  def mirrorGraph(startNode: Exp[_], rewriter: Transformer): Exp[_] = {
    val trans = new SubstTransformer()

    def mirrorNode(node: Exp[_]): Exp[_] = {
      trans.subst.contains(node) match {
        case true => trans(node)
        case _ =>
          node match {
            case Var(_) => {
              val newVar = fresh(node.Elem)
              trans += (node -> newVar)
              newVar
            }
            case Def(d) => {
              val inputNodes = dep(d)
              inputNodes foreach { mirrorNode(_) }
              val newNode = mirror(d, trans)
              val rewritten = rewriter(newNode)
              trans += (node -> rewritten)
              rewritten
            }
          }
      }
    }

    mirrorNode(startNode)
  }

}
