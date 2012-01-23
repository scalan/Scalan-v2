package scalan.staged

import scala.virtualization.lms.util.GraphUtil
import scala.collection._
import scalan.common.Common._

trait PrimitivesLifting { self: StagedImplementation =>
  def vectorizeBinOp[A](op: BinOp[A])(implicit ea: Elem[A]): Rep[A => A => A] = {
    val scalar = mkLambda2((x:Rep[A]) => (y: Rep[A]) => op.copyWith(x, y))
    val lifted = mkLambda2((xs:PA[A]) => (ys:PA[A]) => ExpBinopArray(op, xs, ys))
    val opS = mkLambda2((e:Rep[Unit]) => (x: Rep[A]) => Clo(x, scalar, lifted))
    val opL = mkLambda2((e:PA[Unit]) => (xs: PA[A]) => ExpFuncArray(xs, scalar, lifted))
    Clo((), opS, opL)
  }
}


