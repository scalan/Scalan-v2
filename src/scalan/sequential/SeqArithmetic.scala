package scalan.sequential

import scalan.dsl.ScalanArithmetic
import scalan.common.{Monoid, Semigroup}
import scala.math._

import text.Document

trait SeqArithmetic extends ScalanArithmetic { self: SeqImplBase =>
  import scala.math.Numeric._

  def infix_%[T](x: Rep[T], y: Rep[T])(implicit eT: Elem[T], i: scala.Integral[T]): Rep[T] = i.rem(x,y)
  def infix_/[T](x: Rep[T], y: Rep[T])(implicit eT: Elem[T], i: scala.Integral[T]): Rep[T] = i.quot(x,y)

//  abstract class ImplIntegral[T] extends Integral[T] with scala.math.Integral[T] {
//    def infix_%(x: Rep[T], y: Rep[T])(implicit i: scala.Integral[T]): Rep[T] = x % y
//    def infix_/(x: Rep[T], y: Rep[T])(implicit i: scala.Integral[T]): Rep[T] = x / y
//  }
//  private object floatIntegral extends ImplIntegral[Float] with FloatAsIfIntegral with Ordering.FloatOrdering
//  private object intIntegral extends ImplIntegral[Int] with IntIsIntegral with Ordering.IntOrdering
//  implicit def FloatIsIntegral(ss: Rep[Float]): Integral[Float]#Ops =  floatIntegral.Pimp(ss)(FloatAsIfIntegral)
//  implicit def IntIsIntegral(ss: Rep[Int]): Integral[Int]#Ops =  intIntegral.Pimp(ss)
}
