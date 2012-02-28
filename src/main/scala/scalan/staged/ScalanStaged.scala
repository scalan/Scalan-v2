package scalan.staged

import reflect.Manifest
import virtualization.lms.common.{IfThenElsePureExp, EqualExp}
import scalan.dsl.{ScalanIfThenElse, ScalanEqual, Scalan, Arrays}


trait ScalanStaged
  extends Scalan
  with StagedImplementation
  with StagedEqual
  with StagedViews
  //with StagedTreeImplementation
  with StagedStrings
  //with StagedArithmetic
  with StagedStdArrayOps
  with StagedPArrayOps
  //with CompilerExp
{
  implicit def paManifest[A](implicit ea: Elem[A]): Manifest[PArray[A]] =
      Manifest.classType(classOf[PArray[A]], ea.manifest)
}
