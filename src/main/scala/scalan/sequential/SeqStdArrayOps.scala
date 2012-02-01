package scalan.sequential

import scalan.dsl._

trait SeqStdArrayOps extends ScalanStdArrayOps { self: SeqImplBase =>

  def array_length[T](a: Rep[Array[T]]): Rep[Int] = a.length

  def array_apply[T](a: Rep[Array[T]], n: Rep[Int])(implicit et:Elem[T]): Rep[T] = a(n)
}















