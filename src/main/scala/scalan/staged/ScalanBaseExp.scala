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

  case class Left[A,B](left: Exp[A]) extends Def[(A|B)]
  case class Right[A,B](right: Exp[B]) extends Def[(A|B)]
  case class FoldSum[A,B,R](sum: Exp[(A|B)], left: Exp[A => R], right: Exp[B => R])
                           (implicit val eR: Elem[R])extends Def[R]
  
  class SumOps[A:Elem,B:Elem](s: Rep[(A|B)]) {
    def fold[R:Elem](l: Rep[A] => Rep[R], r: Rep[B] => Rep[R]): Rep[R] = FoldSum(s, fun(l), fun(r))
  }
  implicit def pimpSum[A:Elem, B:Elem](p: Rep[(A|B)]): SumOps[A,B] = new SumOps(p)

  override def rewrite[T](d: Def[T])(implicit eT: Elem[T]) = d match {
    case First(Def(Tup(a,b))) => a
    case Second(Def(Tup(a,b))) => b
    case f@FoldSum(Def(Left(left)), l, _) => {implicit val eR = f.eR; mkApply(l, left)(left.Elem, eR) }
    case f@FoldSum(Def(Right(right)), _, r) => {implicit val eR = f.eR; mkApply(r, right)(right.Elem, eR) }

    case _ => super.rewrite(d)
  }


}








