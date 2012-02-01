package scalan.sequential

import scala.reflect.Manifest
import scalan.dsl._
import virtualization.lms.common.Pure


trait SeqImplBase extends ArraysBase {
  type Rep[+A] = A
  //type Elem[A] = Element[A]

  trait SeqElement[A] extends Element[A] {
    def toRep(x: A): Rep[A] = x
  }

  def unit[A](x: A): Rep[A] = x
  def unzipPair[A, B](p: Rep[(A, B)]): (Rep[A], Rep[B]) = p
  def zipPair[A, B](p: (Rep[A], Rep[B])): Rep[(A, B)] = p

  def fromPA2PArray[A](x: Rep[PArray[A]]): PArray[A] = x

  //implicit def constPure[A]: Pure[A,Rep] = new Pure[A,Rep]{ def pure(x: => A) = x }
  implicit def mkLambda[A,B](fun: Rep[A] => Rep[B])(implicit eA: Elem[A], eB: Elem[B]): Rep[A => B] = fun
  def mkApply[A:Elem,B:Elem](fun: Rep[A => B], arg: Rep[A]): Rep[B] = fun(arg)

  def fix[A,B](f: (A=>B)=>(A=>B)): A=>B = f(fix(f))(_)

  def letrec[A,B](f: (Rep[A=>B])=>(Rep[A]=>Rep[B]))(implicit eA: Elem[A], eb:Elem[B]): Rep[A=>B] = {
    f(letrec(f))(_)
  }

  def __ifThenElse[T](cond: Boolean, thenp: => T, elsep: => T): T = IfThenElseHack.ifThenElse(cond, thenp, elsep)
  def __ifThenElse[T](cond: Rep[Boolean], thenp: => PA[T], elsep: => PA[T])(implicit o: Overloaded1, et: Elem[T]): PA[T] = IfThenElseHack.ifThenElse(cond, thenp, elsep)

  def equals[A:Elem,B:Elem](a: Rep[A], b: Rep[B]): Rep[Boolean] = a equals b
  def notequals[A:Elem,B:Elem](a: Rep[A], b: Rep[B]): Rep[Boolean] = !equals(a,b)

  def compose[A:Manifest, T1, R](f: Rep[T1 => R], g: Rep[A => T1]): Rep[A => R] = (x:Rep[A]) => f(g(x))

  def unzip[A, B](a: PA[(A, B)])(implicit ea: Elem[A], eb: Elem[B]): (PA[A], PA[B]) = a match {
    case pair: PairArray[_,_] => (pair.a, pair.b)
    case _ => sys.error("expected PairArray but was " + a)
  }
  def concat[A:Elem](a: PA[PA[A]]): PA[A] = a match {
    case nested: NestedArray[_] => nested.arr
    case _ => sys.error("expected NestedArray but was " + a)
  }
  def unconcat[A, B](shapeArr: PA[PA[A]])(arr: PA[B])(implicit ea: Elem[A], eb: Elem[B]): PA[PA[B]] = shapeArr match {
    case nested: NestedArray[_] => mkNestedArray(arr, nested.segments)
    case _ => sys.error("expected NestedArray but was " + shapeArr)
  }
  def nestArrays[A:Elem](a1: PA[A], a2: PA[A]): PA[PArray[A]] = {
    implicit val epa = implicitly[Elem[PArray[A]]]
    implicit val mpa = epa.manifest
    epa.fromArray(Array(a1, a2))
  }

}


