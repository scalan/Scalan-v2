package scalan.dsl

import scala.text._
import scalan.common.{Monoid, Semigroup}

trait Arrays extends ArraysBase {
  def length[T: Elem](a: PA[T]): Rep[Int] = a.length
  def fromArray[T](x: Rep[Array[T]])(implicit et: Elem[T]): PA[T] = et.fromArray(x)
  def index[T: Elem](a: PA[T], i: Rep[Int]): Rep[T] = a.index(i)
  def singleton[T:Elem](v: Rep[T]): PA[T] = element[T].singleton(v)
  def replicate[T:Elem](count: Rep[Int], v: Rep[T]): PA[T] = element[T].replicate(count, v)
  def replicateSeg[T:Elem](count: Rep[Int], vs: PA[T]): PA[T] = element[T].replicateSeg(count, vs)
  def tabulate[T:Elem](len:Rep[Int])(f:Rep[Int] => Rep[T]): PA[T] = element[T].tabulate(len)(f)
  def tabulateSeg[T:Elem](len:Rep[Int])(f:Rep[Int] => PA[T]): PA[T] = element[T].tabulateSeg(len)(f)
  def map[A,B:Elem](f: Rep[A] => Rep[B])(a: PA[A]): PA[B] = a.map(f)
  def zip[A,B](a: PA[A], b: PA[B])(implicit ea:Elem[A], eb:Elem[B], eab:Elem[(A,B)]): PA[(A, B)] = a.zip(b)

  // place elements to specified indexes
  def permute[T:Elem](a: PA[T], idxs: PA[Int]): PA[T] = a.permute(idxs)

  // retrieve elements from specified indexes
  def backPermute[T:Elem](a: PA[T], idxs: PA[Int]): PA[T] = a.backPermute(idxs)
  def slice[T](a: PA[T], start: Rep[Int], len: Rep[Int]): PA[T] = a.slice(start, len)

  def flagCombine[A](flags: PA[Boolean], ifTrue: PA[A], ifFalse: PA[A]) = ifTrue.flagCombine(ifFalse, flags)
  def flagMerge  [A](flags: PA[Boolean], ifTrue: PA[A], ifFalse: PA[A]) = ifTrue.flagMerge(ifFalse, flags)
  def flagSplit  [A](flags: PA[Boolean], a: PA[A]) = a.flagSplit(flags)

  // remove all corresponding to false preserving order
  def pack[A](a: PA[A], flags: PA[Boolean]): PA[A] = a.pack(flags)

  def scan[A:Monoid](a: PA[A]): PA[A] = a.scan
  def sum [A](a:PA[A])(implicit m: Monoid[A]): Rep[A] = a.sum(m)

  implicit def rangeToArr(r: Range) = r.toArray
}

