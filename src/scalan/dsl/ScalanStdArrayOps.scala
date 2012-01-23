package scalan.dsl

import scalan.common.PimpedType
import virtualization.lms.ppl.ArrayOps

trait ScalanStdArrayOps extends ScalanBase with ArrayOps { self: ArraysBase =>

//  trait IArrays[A] { arrays =>
//    def length(arr: Rep[Array[A]]): Rep[Int]
//    def index(arr: Rep[Array[A]], i: Rep[Int]): Rep[A]
//
//    trait Wrapper extends PimpedType[Rep[Array[A]]] {
//      def length: Rep[Int] = arrays.length(value)
//      def apply(i: Rep[Int]) = arrays.index(value, i)
//    }
//    def Pimp(arr: Rep[Array[A]]) = new Wrapper { val value = arr  }
//  }
//
//  implicit def pimpArray[A](arr: Rep[Array[A]]): IArrays[A]#Wrapper
}














