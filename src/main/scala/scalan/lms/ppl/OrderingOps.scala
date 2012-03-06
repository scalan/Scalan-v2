package scala.virtualization.lms
package ppl

import scala.virtualization.lms.common._
import scala.virtualization.lms.util.OverloadHack
import java.io.PrintWriter
import scalan.dsl.ArraysBase


trait OrderingOps extends Base with OverloadHack { self: ArraysBase =>
  implicit def repOrderingToOrderingOps[T](x: Rep[T])(implicit n: Ordering[T], et: Elem[T]) = new OrderingOpsCls(x)
  implicit def OrderingToOrderingOps[T](x: T)(implicit n: Ordering[T], et: Elem[T]) = new OrderingOpsCls(x)

  class OrderingOpsCls[T](lhs: Rep[T])(implicit val n: Ordering[T], et: Elem[T]) {
    def <(rhs: Rep[T]) = ordering_lt(lhs, rhs)
    def <=(rhs: Rep[T]) = ordering_lteq(lhs, rhs)
    def >(rhs: Rep[T]) = ordering_gt(lhs, rhs)
    def >=(rhs: Rep[T]) = ordering_gteq(lhs, rhs)
    def equiv(rhs: Rep[T]) = ordering_equiv(lhs, rhs)
    def max(rhs: Rep[T]): Rep[T] = ordering_max(lhs, rhs)
    def min(rhs: Rep[T]): Rep[T] = ordering_min(lhs, rhs)
  }

  def ordering_lt[T](lhs: Rep[T], rhs: Rep[T])(implicit n: Ordering[T]): Rep[Boolean]
  def ordering_lteq[T](lhs: Rep[T], rhs: Rep[T])(implicit n: Ordering[T]): Rep[Boolean]
  def ordering_gt[T](lhs: Rep[T], rhs: Rep[T])(implicit n: Ordering[T]): Rep[Boolean]
  def ordering_gteq[T](lhs: Rep[T], rhs: Rep[T])(implicit n: Ordering[T]): Rep[Boolean]
  def ordering_equiv[T](lhs: Rep[T], rhs: Rep[T])(implicit n: Ordering[T]): Rep[Boolean]
  def ordering_max[T](lhs: Rep[T], rhs: Rep[T])(implicit n: Ordering[T], et: Elem[T]): Rep[T]
  def ordering_min[T](lhs: Rep[T], rhs: Rep[T])(implicit n: Ordering[T], et: Elem[T]): Rep[T]
}


trait OrderingOpsExp extends OrderingOps with BaseExp { self: ArraysBase =>
  case class OrderingLT[T](lhs: Exp[T], rhs: Exp[T], implicit val n: Ordering[T]) extends Def[Boolean]
  case class OrderingLTEQ[T](lhs: Exp[T], rhs: Exp[T], implicit val n: Ordering[T]) extends Def[Boolean]
  case class OrderingGT[T](lhs: Exp[T], rhs: Exp[T], implicit val n: Ordering[T]) extends Def[Boolean]
  case class OrderingGTEQ[T](lhs: Exp[T], rhs: Exp[T], implicit val n: Ordering[T]) extends Def[Boolean]
  case class OrderingEquiv[T](lhs: Exp[T], rhs: Exp[T], implicit val n: Ordering[T]) extends Def[Boolean]
  case class OrderingMax[T](lhs: Exp[T], rhs: Exp[T], implicit val n: Ordering[T]) extends Def[T]
  case class OrderingMin[T](lhs: Exp[T], rhs: Exp[T], implicit val n: Ordering[T]) extends Def[T]

  def ordering_lt[T](lhs: Exp[T], rhs: Exp[T])(implicit n: Ordering[T]): Rep[Boolean] = OrderingLT(lhs,rhs,n)
  def ordering_lteq[T](lhs: Exp[T], rhs: Exp[T])(implicit n: Ordering[T]): Rep[Boolean] = OrderingLTEQ(lhs,rhs,n)
  def ordering_gt[T](lhs: Exp[T], rhs: Exp[T])(implicit n: Ordering[T]): Rep[Boolean] = OrderingGT(lhs,rhs,n)
  def ordering_gteq[T](lhs: Exp[T], rhs: Exp[T])(implicit n: Ordering[T]): Rep[Boolean] = OrderingGTEQ(lhs,rhs,n)
  def ordering_equiv[T](lhs: Exp[T], rhs: Exp[T])(implicit n: Ordering[T]): Rep[Boolean] = OrderingEquiv(lhs,rhs,n)
  def ordering_max[T](lhs: Exp[T], rhs: Exp[T])(implicit n: Ordering[T], et: Elem[T]): Rep[T] = OrderingMax(lhs,rhs,n)
  def ordering_min[T](lhs: Exp[T], rhs: Exp[T])(implicit n: Ordering[T], et: Elem[T]): Rep[T] = OrderingMin(lhs,rhs,n)

  override def rewrite[T](d: Def[T])(implicit eT: Elem[T]) = d match {
    case OrderingLT(Def(Const(x)), Def(Const(y)), n) => Const(n.lt(x,y))
    case OrderingGT(Def(Const(x)), Def(Const(y)), n) => Const(n.gt(x,y))
    case OrderingLTEQ(Def(Const(x)), Def(Const(y)), n) => Const(n.lteq(x,y))
    case OrderingGTEQ(Def(Const(x)), Def(Const(y)), n) => Const(n.gteq(x,y))
    case _ => super.rewrite(d)
  }
}

trait ScalaGenOrdering extends ScalaGenBase  { this: OrderingOpsExp with ArraysBase =>

  abstract override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case OrderingLT(a,b,n) => emitValDef(sym, quote(a) + " < " + quote(b))
    case OrderingLTEQ(a,b,n) => emitValDef(sym, quote(a) + " <= " + quote(b))
    case OrderingGT(a,b,n) => emitValDef(sym, quote(a) + " > " + quote(b))
    case OrderingGTEQ(a,b,n) => emitValDef(sym, quote(a) + " >= " + quote(b))
    case OrderingEquiv(a,b,n) => emitValDef(sym, quote(a) + " equiv " + quote(b))
    case OrderingMax(a,b,n) => emitValDef(sym, quote(a) + " max " + quote(b))
    case OrderingMin(a,b,n) => emitValDef(sym, quote(a) + " min " + quote(b))
    case _ => super.emitNode(sym, rhs)
  }
}
