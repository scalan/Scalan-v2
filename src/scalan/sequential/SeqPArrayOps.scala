package scalan.sequential

import scalan.dsl.{ArraysBase, PArrayOps}
import scalan.common.{Monoid, Semigroup}
import text.Document

trait SeqPArrayOps extends PArrayOps { self: SeqImplBase =>
  override implicit def pimpPairArray[A:Elem, B:Elem](p: PA[(A,B)]): PairArrayOps[A,B] = ???

  override implicit def pimpNestedArray[A:Elem](nested: PA[PArray[A]]): NestedArrayOps[A] = ???

  override implicit def pimpFuncArray[A:Elem,B:Elem](f: PA[A => B]): FuncArrayOps[A, B] = ???
}
