package scala.virtualization.lms
package ppl

import scala.virtualization.lms.common._
import java.io.{BufferedReader, FileReader, PrintWriter}
import scalan.dsl.ArraysBase
import scalan.common.Monoid

trait ArrayOps { self: ArraysBase =>

  implicit def repArrayToRepArrayOps[T:Elem](a: Rep[Array[T]]) = new RepArrayOpsCls(a)
  implicit def arrayToRepArrayOps[T:Elem](a: Array[T]) = {
    implicit val m = element[T].manifest
    //implicit val arrElem = element[Array[T]]
    new RepArrayOpsCls(toRep(a))
  }

  class RepArrayOpsCls[T](a: Rep[Array[T]])(implicit et: Elem[T]){
    def apply(n: Rep[Int]) = array_apply(a, n)
    def length = array_length(a)
  }

  def array_apply[T](x: Rep[Array[T]], n: Rep[Int])(implicit et:Elem[T]): Rep[T]
  def array_length[T](a: Rep[Array[T]]) : Rep[Int]

}

trait ArrayOpsExp extends ArrayOps with BaseExp { self: ArraysBase =>
  case class ArrayLength[T](a: Exp[Array[T]]) extends Def[Int]
  case class ArrayApply[T](x: Exp[Array[T]], n: Exp[Int]) extends Def[T]
  case class ArraySum[T](x: Exp[Array[T]], implicit val m: Monoid[T]) extends Def[T]

  def array_apply[T](x: Exp[Array[T]], n: Exp[Int])(implicit et:Elem[T]): Rep[T] = ArrayApply(x, n)
  def array_length[T](a: Exp[Array[T]]) : Rep[Int] = ArrayLength(a)


}

trait ScalaGenArray extends ScalaGenEffect { this: ArrayOpsExp with ArraysBase =>

  abstract override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {    
    case ArrayLength(x) => emitValDef(sym, "" + quote(x) + ".length")
    case ArrayApply(x,n) => emitValDef(sym, "" + quote(x) + "(" + quote(n) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}
