package scalan.dsl

import virtualization.lms.common._

trait ScalanTuples extends ScalanBase {
  implicit def zipPair[A,B](p: (Rep[A], Rep[B])): Rep[(A,B)]
  def unzipPair[A,B](p: Rep[(A,B)]): (Rep[A], Rep[B])

  implicit object Pair {
    def apply[A,B](a: Rep[A], b: Rep[B]) = zipPair((a,b))
    implicit def unapply[A,B](p: Rep[(A,B)]) = Some(unzipPair(p))
  }
}












