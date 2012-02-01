package scalan.staged

import java.io.PrintWriter
import virtualization.lms.util.OverloadHack
//import virtualization.lms.ppl.ImplicitOps
import scalan.common.Monoid
import virtualization.lms.common.{ScalaGenBase, BaseExp, Base}
import scalan.dsl.{ArraysBase, ScalanBase}

trait MonoidOps extends ScalanBase
      with OverloadHack
      //with ImplicitOps
{ self: ArraysBase =>

  def monoid_append[T](lhs: Rep[T], rhs: Rep[T])(implicit f: Monoid[T], et: Elem[T]): Rep[T]
}

trait MonoidOpsExp extends MonoidOps with ScalanBaseExp {
  case class MonoidAppend[T](lhs: Exp[T], rhs: Exp[T], implicit val f: Monoid[T]) extends Def[T]

  def monoid_append[T](lhs: Exp[T], rhs: Exp[T])(implicit f: Monoid[T], et: Elem[T]) : Rep[T] = MonoidAppend(lhs, rhs, f)
}

trait ScalaGenMonoid extends ScalaGenBase  { this: MonoidOpsExp =>

  abstract override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case MonoidAppend(a,b,f) => emitValDef(sym, "implicitly[Monoid[_]].append(" + quote(a) + ", " + quote(b) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}

