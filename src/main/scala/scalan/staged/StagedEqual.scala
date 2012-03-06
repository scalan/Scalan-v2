package scalan.staged

import reflect.Manifest
import virtualization.lms.common.{IfThenElsePureExp, EqualExp}
import scalan.dsl.{ArraysBase, ScalanEqual, Scalan, Arrays}

trait StagedEqual extends ScalanEqual with EqualExp { self: ArraysBase =>
  override def rewrite[T](d: Def[T])(implicit eT: Elem[T]) = d match {
    case Equal(Def(Const(x)), Def(Const(y))) => Const(x equals y)
    case NotEqual(Def(Const(x)), Def(Const(y))) => Const(!(x equals y))
    case _ => super.rewrite(d)
  }

}



