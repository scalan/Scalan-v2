package scalan.staged

import reflect.Manifest
import virtualization.lms.common.{IfThenElsePureExp, EqualExp}
import scalan.dsl._

trait StagedIfThenElse extends ScalanIfThenElse with IfThenElsePureExp { self: ArraysBase =>

  override def rewrite[T](d: Def[T]): Def[_] = d match {
    case IfThenElse(Def(Const(x)), Def(thenp), Def(elsep)) => x match { case true => thenp case _ => elsep }
    case _ => super.rewrite(d)
  }

}


