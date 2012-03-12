package scalan.dsl

import text.Document
import scalan.common.{Monoid, Semigroup, PimpedType}

trait PArrayOps extends ScalanBase { self: ArraysBase =>

  trait BaseArrayOps[A] {
    def ArrayElem: PArrayElem[A]
    def collectLeft[B:Elem,C](f: Rep[A => (B|C)]): PA[B]
    def collectRight[B,C:Elem](f: Rep[A => (B|C)]): PA[C]
    def mapSplit[B:Elem,C:Elem](f: Rep[A => (B|C)]): PA[(B|C)]
  }
  implicit def pimpBaseArray[A](p: PA[A]): BaseArrayOps[A]

  trait PairArrayOps[A,B] {
    def fst: PA[A]
    def snd: PA[B]
  }
  implicit def pimpPairArray[A,B](p: PA[(A,B)])(implicit eA:Elem[A], eB:Elem[B]): PairArrayOps[A,B]

  trait SumArrayOps[A,B] {
    def flags: PA[Boolean]
    def a: PA[A]
    def b: PA[B]
  }
  implicit def pimpSumArray[A,B](p: PA[(A|B)])(implicit eA:Elem[A], eB:Elem[B]): SumArrayOps[A,B]

  trait NestedArrayOps[A] {
    def values: PA[A]
    def segments: PA[(Int,Int)]
    def unzipNested: (PA[A], PA[(Int,Int)])
    def indexLifted(idxs: PA[Int]): PA[A]
    def sumLifted[B >: A <: A](implicit m: Monoid[B]): PA[B]
    def mapLifted[B:Elem](f: PA[A=>B]): PA[PArray[B]]
    def zipLifted[B:Elem](b: PA[PArray[B]]): PA[PArray[(A,B)]]
  }
  implicit def pimpNestedArray[A:Elem](nested: PA[PArray[A]]): NestedArrayOps[A]

  trait FuncArrayOps[A, B] {
    def apply(x: PA[A]): PA[B]
  }
  implicit def pimpFuncArray[A:Elem,B:Elem](f: PA[A => B]): FuncArrayOps[A, B]
}













