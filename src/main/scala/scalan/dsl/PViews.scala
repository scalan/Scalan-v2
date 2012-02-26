package scalan.dsl

import scalan.common.Zero
import scala.text._
import Document._
import scalan.util.DocumentExtensions._

trait PViews extends ArraysBase {

  trait Iso[A,B] {
    def from: Rep[B] => Rep[A]
    def to: Rep[A] => Rep[B]
    def manifest: Manifest[B]
    def zero: Zero[B]
  }

  implicit def viewElement[A,B](implicit iso: Iso[A,B], ea: Elem[A]): Elem[B]

  trait ViewArray[A, B] extends PArray[B] {
    def a: PA[A]
    def iso: Iso[A, B]
    def length = a.length
    def index(i: IntRep) = iso.to(a(i))
    override def toDoc = group("ViewArray(" :: nest(2,a.toDoc) :/: ")" :: ED)
  }


}
