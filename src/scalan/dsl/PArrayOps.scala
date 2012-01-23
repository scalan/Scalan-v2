package scalan.dsl

import text.Document
import scalan.common.{Monoid, Semigroup, PimpedType}

trait PArrayOps extends ScalanBase { self: ArraysBase =>

  trait PairArrayOps[A,B] {
    def fst: PA[A]
    def snd: PA[B]
  }
  implicit def pimpPairArray[A,B](p: PA[(A,B)])(implicit eA:Elem[A], eB:Elem[B]): PairArrayOps[A,B]

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













