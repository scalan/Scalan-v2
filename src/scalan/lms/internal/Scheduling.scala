package scala.virtualization.lms
package internal

import util.GraphUtil
import scalan.dsl.ArraysBase

trait Scheduling extends Expressions { self: ArraysBase =>
  def buildScheduleForResult(start: Exp[_]): List[TP[_]] = {
    val st = syms(start)
    GraphUtil.stronglyConnectedComponents[TP[_]](st.flatMap(e => findDefinition(e).toList), { d =>
      //println("dep"+d +"="+dep(d.rhs))
      dep(d.sym).flatMap { e =>
        //println(d + "->" + e)
        findDefinition(e).toList
      }
    }).flatten.reverse // inefficient!
  }
}
