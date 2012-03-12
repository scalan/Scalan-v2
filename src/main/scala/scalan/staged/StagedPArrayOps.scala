package scalan.staged

import scalan.common.{Monoid, Semigroup}
import text.Document
import scalan.dsl.{Arrays, PArrayOps}
import reflect.SourceContext
import scala.virtualization.lms.common.BaseExp
import scala.virtualization.lms.internal.Expressions

trait StagedPArrayOps extends BaseExp
                         with Expressions
                         with PArrayOps
                         with PrimitivesLifting
                         with Arrays { self: StagedImplementation =>

  case class FirstPA[A,B](source: PA[(A,B)])(implicit val eA: Elem[A]) extends ExpStubArray[A]
  case class SecondPA[A,B](source: PA[(A,B)])(implicit val eB: Elem[B]) extends ExpStubArray[B]

  case class NestedArrayValues[A](nested: PA[PArray[A]])(implicit eA: Elem[A]) extends ExpStubArray[A]
  case class NestedArraySegments[A:Elem](nested: PA[PArray[A]]) extends ExpStubArray[(Int,Int)]

  case class RangePA(start: IntRep, len: IntRep) extends ExpStubArray[Int]
  case class GetLeftPA[A:Elem, B](sums: PA[(A|B)]) extends ExpStubArray[A]
  case class GetRightPA[A, B:Elem](sums: PA[(A|B)]) extends ExpStubArray[B]
  case class FlagsPA[A, B](sums: PA[(A|B)]) extends ExpStubArray[Boolean]
  case class CollectLeftPA[A,B:Elem,C](xs: PA[A], func: Rep[A => (B|C)]) extends ExpStubArray[B]
  case class CollectRightPA[A,B,C:Elem](xs: PA[A], func: Rep[A => (B|C)]) extends ExpStubArray[C]
  case class MapSplit[A,B:Elem,C:Elem](xs: PA[A], func: Rep[A => (B|C)]) extends ExpStubArray[(B|C)]

  class StagedBaseArrayOps[A](arr: PA[A]) extends BaseArrayOps[A] {
    def ArrayElem: PArrayElem[A] = arr.Elem.asInstanceOf[PArrayElem[A]]
    def collectLeft[B:Elem,C](f: Rep[A => (B|C)]): PA[B] = CollectLeftPA(arr, f)
    def collectRight[B,C:Elem](f: Rep[A => (B|C)]): PA[C] = CollectRightPA(arr, f)
    def mapSplit[B:Elem,C:Elem](f: Rep[A => (B|C)]): PA[(B|C)] = MapSplit(arr, f)
  }
  implicit def pimpBaseArray[A](p: PA[A]): BaseArrayOps[A] = new StagedBaseArrayOps(p)

  class StagedPairArrayOps[A:Elem,B:Elem](p: PA[(A,B)]) extends PairArrayOps[A,B] {
    def fst: PA[A] = FirstPA(p)
    def snd: PA[B] = SecondPA(p)
  }
  implicit def pimpPairArray[A,B](p: PA[(A,B)])(implicit eA:Elem[A], eB:Elem[B]): PairArrayOps[A,B] = new StagedPairArrayOps(p)

  class StagedSumArrayOps[A:Elem,B:Elem](p: PA[(A|B)]) extends SumArrayOps[A,B] {
    def flags: PA[Boolean] = FlagsPA(p)
    def a: PA[A] = GetLeftPA(p)
    def b: PA[B] = GetRightPA(p)
  }
  implicit def pimpSumArray[A,B](p: PA[(A|B)])(implicit eA:Elem[A], eB:Elem[B]): SumArrayOps[A,B] = new StagedSumArrayOps(p)

  class StagedNestedArrayOps[A:Elem](nested: PA[PArray[A]]) extends NestedArrayOps[A] {
    def values: PA[A] = NestedArrayValues(nested)
    def segments: PA[(Int,Int)] = NestedArraySegments(nested)

    def unzipNested: (PA[A], PA[(Int,Int)]) = (nested.values, nested.segments)

    def indexLifted(idxs: PA[Int]): PA[A] = IndexLiftedPA(nested, idxs)
    def sumLifted[B >: A <: A](implicit m: Monoid[B]): PA[B] = SumLiftedPA(nested, m)

    def mapLifted[B:Elem](f: PA[A=>B]): PA[PArray[B]] = f match {
      case Def(ExpFuncArray(env, _, fl)) =>
        implicit val envElem = env.Elem
        unconcat(nested)(fl(env expandBy nested)(concat(nested)))
      case _ => MapLiftedPA(nested, f)
    }

    def zipLifted[B:Elem](b: PA[PArray[B]]): PA[PArray[(A,B)]] = {
      val (xs, ys, segs) = nested match {
        case Def(ExpNestedArray(xs, segs)) => b match {
          case Def(ExpNestedArray(ys, _)) => (xs, ys, segs)
          case _ => (xs, b.values, segs)
        }
        case Var(_) => b match {
          case Def(ExpNestedArray(ys, segs)) => (nested.values, ys, segs)
          case _ => (nested.values, b.values, nested.segments)
        }
      }
      ExpNestedArray(xs zip ys, segs)
    }

    lazy val intPlusV = vectorizeBinOp(NumericPlus(0,0, implicitly[Numeric[Int]]))
    lazy val intMinusV = vectorizeBinOp(NumericMinus(0,0, implicitly[Numeric[Int]]))

  }
  implicit def pimpNestedArray[A:Elem](nested: PA[PArray[A]]): NestedArrayOps[A] = new StagedNestedArrayOps(nested)

//  class NestedArrayMonoidOps[A:Elem](nested: PA[PArray[A]], m: Monoid[A]) {
//    def sumLifted[A](implicit eB:Elem[], m: Monoid[B]): PA[B] = SumLiftedPA(nested, m)
//  }
//  implicit def pimpNestedArrayWithMonoid[A]
//      (nested: PA[PArray[A]])
//      (implicit eA:Elem[A], m: Monoid[A]): NestedArrayMonoidOps[A] = new NestedArrayMonoidOps(nested, m)

  class StagedFuncArrayOps[A:Elem, B:Elem](f: PA[A => B]) extends FuncArrayOps[A,B] {
    def apply(x: PA[A]): PA[B] = doApplyPA(f, x)
  }
  implicit def pimpFuncArray[A:Elem,B:Elem](f: PA[A => B]): FuncArrayOps[A, B] = new StagedFuncArrayOps(f)

  def unzip[A, B](p: PA[(A, B)])(implicit ea: Elem[A], eb: Elem[B]): (PA[A], PA[B]) = p match {
    case Def(ExpPairArray(a,b)) => (a, b)
    case _ => p.Elem match {
      case pae: PArrayElem[_] => (FirstPA(p), SecondPA(p))   //FIXME: implement using extractors
      case _ =>
        !!!("expected PairArray or Sym with type PArray[(A,B)] but was " + p.toString, p)
    }
  }

  def concat[A:Elem](a: PA[PArray[A]]): PA[A] = a match {
    case Def(ExpNestedArray(arr, _)) => arr
    case _ => a.values
    //case _ => sys.error("expected NestedArray but was " + a.toString)
  }
  def unconcat[A, B](shapeArr: PA[PArray[A]])(arr: PA[B])
                    (implicit ea: Elem[A], eb: Elem[B]): PA[PArray[B]] = shapeArr match {
    case Def(ExpNestedArray(_, segs)) => mkNestedArray(arr, segs)
    case _ => mkNestedArray(arr, shapeArr.segments)
    //case _ => sys.error("expected NestedArray but was " + shapeArr.toString)
  }
  def nestArrays[A:Elem](a1: PA[A], a2: PA[A]):PA[PArray[A]] = NestArrays(a1, a2)

  override def mirror[A](d: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[_] = d match {
    case x@FirstPA(pa) => { implicit val elem = x.eA; FirstPA(f(pa)) }
    case x@SecondPA(pa) => { implicit val elem = x.eB; FirstPA(f(pa)) }
    case _ => super.mirror(d, f)
  }

  override def rewrite[T](d: Def[T])(implicit eT: Elem[T]) = d match {
    case FirstPA(Def(ExpPairArray(a,b))) => a
    case SecondPA(Def(ExpPairArray(a,b))) => b
    case NestedArrayValues(Def(ExpNestedArray(xs, _))) => xs
    case NestedArraySegments(Def(ExpNestedArray(_, segs))) => segs

    case _ => super.rewrite(d)
  }
}
