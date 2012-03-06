package scalan.staged

import scalan.dsl._

trait StagedArithmetic extends ScalanArithmetic with ScalanExpressions { self: StagedImplementation =>
  abstract class IntegralBinOp[T](i: scala.Integral[T]) extends BinOp[T]

  case class FractionalMod[T](lhs: Exp[T], rhs: Exp[T], implicit val i: scala.Integral[T])
    extends IntegralBinOp[T](i) {
    def copyWith(l: Rep[T], r: Rep[T]) = this.copy(lhs = l, rhs = r)
    override def name = "%"
  }
  case class FractionalDiv[T](lhs: Exp[T], rhs: Exp[T], implicit val i: scala.Integral[T])
    extends IntegralBinOp[T](i) {
    def copyWith(l: Rep[T], r: Rep[T]) = this.copy(lhs = l, rhs = r)
    override def name = "/"
  }

  def infix_%[T](x: Exp[T], y: Exp[T])(implicit eT: Elem[T], i: scala.Integral[T]): Exp[T] = FractionalMod(x, y, i)
  def infix_/[T](x: Exp[T], y: Exp[T])(implicit eT: Elem[T], i: scala.Integral[T]): Exp[T] = FractionalDiv(x, y, i)

  override def rewrite[T](d: Def[T])(implicit eT: Elem[T]): Rep[_] = d match {
    case FractionalMod(Def(Const(x)), Def(Const(y)), i) => {
      //implicit val eT = e.Elem
      Const(i.quot(x, y))
    }
    case NumericTimes(Def(NumericTimes(y, x1, n1)), x2, n2) if (x1 equals x2) && n1 == n2 =>
      //implicit val eT = y.Elem
      NumericTimes(y, NumericTimes(x1, x1, n1), n1)
    case NumericTimes(x2, Def(NumericTimes(x1, y, n1)), n2) if (x1 equals x2) && n1 == n2 =>
      //implicit val eT = y.Elem
      NumericTimes(y, NumericTimes(x1, x1, n1), n1)
    case _ => super.rewrite(d)
  }
}














