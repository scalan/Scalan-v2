package scala.virtualization.lms
package ppl

import scala.virtualization.lms.common._
import scala.virtualization.lms.util.OverloadHack
import java.io.PrintWriter
import scalan.dsl.ArraysBase


trait FractionalOps extends Base
      with OverloadHack
      //with ImplicitOps
{ self: ArraysBase =>

  implicit def repFractionalToFractionalOps[T](x: Rep[T])(implicit f: Fractional[T], et: Elem[T]) = new FractionalOpsCls(x)
  implicit def fractionalToFractionalOps[T](x: T)(implicit f: Fractional[T], et: Elem[T]) = new FractionalOpsCls(x)

  class FractionalOpsCls[T](lhs: Rep[T])(implicit val f: Fractional[T], et: Elem[T]) {
    def /(rhs: Rep[T]) : Rep[T] = fractional_divide(lhs,rhs)
    // TODO: why does this not work, but the uncommented one does?
    //def /[A](rhs: Rep[A])(implicit c: A => T) : Rep[T] = fractional_divide(lhs,implicit_convert[A,T](rhs))
    //def /(rhs: Rep[Int])(implicit c: Int => T) : Rep[T] = fractional_divide(lhs,implicit_convert[Int,T](rhs))
  }

  def fractional_divide[T](lhs: Rep[T], rhs: Rep[T])(implicit f: Fractional[T], et: Elem[T]): Rep[T]
}

trait FractionalOpsExp extends FractionalOps with BaseExp { self: ArraysBase =>
  case class FractionalDivide[T](lhs: Exp[T], rhs: Exp[T], implicit val f: Fractional[T]) extends Def[T]
  
  def fractional_divide[T](lhs: Exp[T], rhs: Exp[T])(implicit f: Fractional[T], et: Elem[T]) : Rep[T] = FractionalDivide(lhs, rhs, f)

  override def rewrite[T](d: Def[T])(implicit eT: Elem[T]): Rep[_] = d match {
    case FractionalDivide(Def(Const(x)), Def(Const(y)), f) => {
      Const(f.div(x, y))
    }
    case _ => super.rewrite(d)
  }

}

trait ScalaGenFractional extends ScalaGenBase  { this: FractionalOpsExp with ArraysBase =>

  abstract override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case FractionalDivide(a,b,f) => emitValDef(sym, quote(a) + " / " + quote(b))
    case _ => super.emitNode(sym, rhs)
  }
}
