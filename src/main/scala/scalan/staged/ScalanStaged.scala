package scalan.staged

import reflect.Manifest
import scalan.lms.common.ProxyExp
import scalan.dsl.Scalan

trait ScalanStaged
  extends Scalan
  with StagedImplementation
  with StagedEqual
  with StagedViews
 // with DomainTypes
  with ProxyExp
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
