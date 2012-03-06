package scalan.dsl

import scalan.common.Zero
import scala.text._
import Document._
import scalan.util.DocumentExtensions._

trait PViews extends ArraysBase {

  trait IsoBase[A,B] {
    def from: B => A
    def to: A => B
    def manifest: Manifest[B]
    def zero: Zero[B]
  }
  type Iso[A,B] <: IsoBase[A,B]

  implicit def viewElement[A,B](implicit iso: Iso[A,B], ea: Elem[A]): Elem[B]

  trait ViewArray[A, B] extends PArray[B] {
    def arr: PA[A]
    def iso: Iso[A, B]
    def length = arr.length
    override def toDoc = group("ViewArray(" :: nest(2,arr.toDoc) :/: ")" :: ED)
  }


}
