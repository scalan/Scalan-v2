package scalan.dsl

import scalan.common.Zero
import scala.text._
import Document._
import scalan.util.DocumentExtensions._

trait PViews extends ArraysBase {

  trait GenIso[A,B] {
    def eA: Elem[A]
    def eB: Elem[B]
    def from: B => A
    def to: A => B
    def manifest: Manifest[B]
    def zero: Zero[B]
    def fromStaged: Rep[B] => Rep[A]
    def toStaged: Rep[A] => Rep[B]
  }
  
  abstract class IsoBase[A,B](implicit val eA: Elem[A]) extends GenIso[A,B] { ///self: Iso[A,B] =>
    lazy val eB: Elem[B] = viewElement(this)
    def from: B => A = (x: B) => ???
    def to: A => B = (x: A) => ???
    def fromStaged: Rep[B] => Rep[A] = (x: Rep[B]) => ???("Not implemented ", x)
    def toStaged: Rep[A] => Rep[B] = (x:Rep[A]) => ???("Not implemented ", x)
  }
  type Iso[A,B] = GenIso[A,B]

  implicit def viewElement[A,B](implicit iso: Iso[A,B]): Elem[B]

  trait ViewArray[A, B] extends PArray[B] {
    def arr: PA[A]
    def iso: Iso[A, B]
    def length = arr.length
    override def toDoc = group("ViewArray(" :: nest(2,arr.toDoc) :/: ")" :: ED)
  }

  trait IdentityIso[A] extends GenIso[A,A] { //self: Iso[A,A] =>
    override def from = (x: A) => x
    override def to = (x: A) => x
    override def fromStaged = (x: Rep[A]) => x
    override def toStaged = (x: Rep[A]) => x
  }
}
