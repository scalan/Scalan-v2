package scalan.dsl

import scalan.common.PimpedType
import virtualization.lms.common.{IfThenElse, StringOps, Equal, Functions}
import virtualization.lms.common._

trait ScalanEqual extends Equal with ScalanLogical { self: ArraysBase =>
}

trait ScalanIfThenElse extends IfThenElse { self: ArraysBase =>
}

trait ScalanFunctions extends ScalanBase with Functions { self: ArraysBase =>
}

trait ScalanArithmetic extends ScalanBase  { self: ArraysBase =>

  def infix_%[T](x: Rep[T], y: Rep[T])(implicit eT: Elem[T], i: scala.Integral[T]): Rep[T]
  def infix_/[T](x: Rep[T], y: Rep[T])(implicit eT: Elem[T], i: scala.Integral[T]): Rep[T]
//  trait Integral[T] {
//    def infix_%(x: Rep[T], y: Rep[T])(implicit i: scala.Integral[T]): Rep[T]
//    def infix_/(x: Rep[T], y: Rep[T])(implicit i: scala.Integral[T]): Rep[T]
//
//    class Ops(override val value: Rep[T])(implicit i: scala.Integral[T]) extends PimpedType[Rep[T]] {
//      def %(y: Rep[T]): Rep[T] = infix_%(value, y)
//      def /(y: Rep[T]): Rep[T] = infix_/(value, y)
//    }
//    def Pimp(ss: Rep[T])(implicit i: scala.Integral[T]) = new Ops(ss)
//  }
//
//  implicit def IntIsIntegral(ss: Rep[Int]): Integral[Int]#Ops
//  implicit def FloatIsIntegral(ss: Rep[Float]): Integral[Float]#Ops
}

trait ScalanStrings extends ScalanBase with StringOps { self: ArraysBase =>
}

trait ScalanLogical extends ScalanBase {
  def infix_&&(x: Rep[Boolean], y: Rep[Boolean]): Rep[Boolean]
  def infix_||(x: Rep[Boolean], y: Rep[Boolean]): Rep[Boolean]
  def unary_not(x: Rep[Boolean]): Rep[Boolean]

  trait RepE extends PimpedType[Rep[Boolean]] {
    def &&(y: Rep[Boolean]): Rep[Boolean] = infix_&&(value, y)
    def ||(y: Rep[Boolean]): Rep[Boolean] = infix_||(value, y)
    def unary_!() : Rep[Boolean] = unary_not(value)
  }
  implicit def RepTo(b: Rep[Boolean]): RepE = new RepE { val value = b  }
}

trait ScalanZipped extends ArraysBase {
  trait TuplePA[A,B] extends PimpedType[(PA[A],PA[B])] {
    def zippedPA(implicit eb:Elem[B], eab: Elem[(A,B)]): Zipped[A, B] = new Zipped[A,B](value._1, value._2)

    class Zipped[A,B](a: PA[A], b: PA[B])(implicit eb:Elem[B], eab: Elem[(A,B)]) {
      def map[C:Elem](f: Rep[(A, B)] => Rep[C]): PA[C] = a.zip(b).map(f)
    }
  }
  implicit def toTuplePA[A,B](p: (PA[A],PA[B])) = new TuplePA[A,B] { val value = p  }

}
