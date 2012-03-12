package scalan.staged

import scalan.dsl._
import virtualization.lms.common.BaseExp


trait StagedSumExp extends BaseExp
                      with ArraysBase { self: StagedImplementation =>

  case class Left[A, B](left: Exp[A]) extends Def[(A | B)]

  case class Right[A, B](right: Exp[B]) extends Def[(A | B)]

  case class IsLeft[A, B](sum: Exp[(A | B)]) extends Def[Boolean]

  case class IsRight[A, B](sum: Exp[(A | B)]) extends Def[Boolean]

  case class FoldSum[A, B, R](sum: Exp[(A | B)], left: Exp[A => R], right: Exp[B => R])
                             (implicit val eR: Elem[R]) extends Def[R]

  case class FoldSumPA[A, B, R](sum: Exp[(A | B)],
                                left: Exp[A => PArray[R]],
                                right: Exp[B => PArray[R]])
                               (implicit val eR: Elem[R]) extends ExpStubArray[R]

  class SumOps[A: Elem, B: Elem](s: Rep[(A | B)]) {
    def fold[R: Elem](l: Rep[A] => Rep[R], r: Rep[B] => Rep[R]): Rep[R] = {
      val res: Rep[_] = element[R] match {
        case epaR: PArrayElem[_] =>
          FoldSumPA(s, fun(l).as[A => PArray[R]], fun(r).as[B => PArray[R]])
        case _ => FoldSum(s, fun(l), fun(r))
      }
      res.as[R]
    }

    def isLeft = IsLeft(s)

    def isRight = IsRight(s)
  }

  implicit def pimpSum[A: Elem, B: Elem](p: Rep[(A | B)]): SumOps[A, B] = new SumOps(p)

  override def rewrite[T](d: Def[T])(implicit eT: Elem[T]) = d match {
    case f@FoldSum(Def(Left(left)), l, _) => {
      implicit val eR = f.eR; mkApply(l, left)(left.Elem, eR)
    }
    case f@FoldSum(Def(Right(right)), _, r) => {
      implicit val eR = f.eR; mkApply(r, right)(right.Elem, eR)
    }
    case IsLeft(Def(lr)) => lr.isInstanceOf[Left[_, _]]
    case IsRight(Def(lr)) => lr.isInstanceOf[Right[_, _]]

    case _ => super.rewrite(d)
  }
}








