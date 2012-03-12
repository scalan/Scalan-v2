package scalan.staged

import scalan.common._
import Common._
import scalan.dsl._
import virtualization.lms.common.{BaseExp, Pure}

trait ScalanBaseExp extends BaseExp with ArraysBase {

  def unzipPair[A, B](p: Rep[(A, B)]): (Rep[A], Rep[B]) = p match {
    case Def(Tup(a, b)) => (a, b)
    case _ => p.Elem match {
      case pe: PairElem[_,_] =>
        implicit val eA = pe.ea
        implicit val eB = pe.eb
        (First(p), Second(p))
      case _ =>
        !!!("expected Tup[A,B] or Sym with type (A,B) but was " + p.toString, p)
    }
  }

  implicit def zipPair[A, B](p: (Exp[A], Exp[B])): Rep[(A, B)] = {
    implicit val ea = p._1.Elem.asInstanceOf[Elem[A]]
    implicit val eb = p._2.Elem.asInstanceOf[Elem[B]]
    Tup(p._1, p._2)
  }

  class PairOps[A:Elem,B:Elem](p: Rep[(A,B)]) {
    def _1: Rep[A] = { val (a, _) = unzipPair(p); a }
    def _2: Rep[B] = { val (_, b) = unzipPair(p); b }
  }
  implicit def pimpPair[A:Elem, B:Elem](p: Rep[(A,B)]): PairOps[A,B] = new PairOps(p)

  case class Tup[A,B](a: Exp[A], b: Exp[B]) extends Def[(A,B)]
  case class First[A,B](pair: Exp[(A,B)]) extends Def[A]
  case class Second[A,B](pair: Exp[(A,B)]) extends Def[B]


  override def rewrite[T](d: Def[T])(implicit eT: Elem[T]) = d match {
    case First(Def(Tup(a,b))) => a
    case Second(Def(Tup(a,b))) => b

    case _ => super.rewrite(d)
  }
}









