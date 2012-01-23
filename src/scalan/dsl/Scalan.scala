package scalan.dsl

import virtualization.lms.ppl._

trait Scalan extends ArraysBase
                with Arrays
                with ScalanEqual
                //with PTrees
                with ScalanStrings
                with ScalanArithmetic
                with ScalanFunctions
                with ScalanLogical
                with ScalanStdArrayOps
                with PArrayOps
                with ScalaOpsPkg
{
  implicit def paManifest[A](implicit ea: Elem[A]): Manifest[PArray[A]]
  //implicit def toRep[A:Elem](x: A): Rep[A]
}
