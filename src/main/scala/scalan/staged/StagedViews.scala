package scalan.staged

import scalan.common.Zero
import scalan.dsl.PViews
import scalan.util.Utils._
import scala.virtualization.lms.internal.Expressions

trait StagedViews extends PViews
                  with StagedImplBase
                  with Expressions { self: StagedImplementation =>

  trait GenStagedIso[A,B] extends GenIso[A,B] {
  }

  //type Iso[A,B] = GenStagedIso[A,B]
  abstract class StagedIso[A,B](implicit eA: Elem[A]) extends IsoBase[A,B] with GenStagedIso[A,B] {
  }

  override implicit def viewElement[A, B](implicit iso: Iso[A, B]): Elem[B] =
    new StagedElement[B] {
      implicit val elemB = this
      implicit private def eA = iso.eA
      //implicit private def eB = iso.eB
      implicit private lazy val m = iso.manifest
      private lazy val z = iso.zero

      def manifest: Manifest[B] = m
      def zero = z

      def replicate(count: IntRep, v: Rep[B]) =
        ExpViewArray(eA.replicate(count, iso.fromStaged(v)), iso)

      def replicateSeg(count: IntRep, v: PA[B]) = v match {
        case Def(ExpViewArray(arr, iso1))
             if iso1 == iso => ExpViewArray(eA.replicateSeg(count, arr.asInstanceOf[PA[A]]), iso)
        case _ =>
          ExpViewArray(eA.replicateSeg(count, ArrayFromView(v, iso)), iso)
      }

      def tabulate(len: IntRep)(f: IntRep => Rep[B]) = {
        val arr = eA.tabulate(len)(iso.fromStaged compose f)
        ExpViewArray(arr, iso)
      }

      def tabulateSeg(len: IntRep)(f: IntRep => PA[B]) = {
        val fa = (i: IntRep) => {
          val segB = f(i); segB.asInstanceOf[ViewArray[A, B]].arr
        }
        val arr = eA.tabulateSeg(len)(fa)
        ExpViewArray(arr, iso)
      }

      def empty = ExpViewArray(eA.empty, iso)
      def toRep(p: B) = iso.toStaged(eA.toRep(iso.from(p)))
    }

  case class ExpViewArray[A, B]
    (arr: PA[A], iso: Iso[A, B])
    //(implicit val eA: Elem[A], val eB: Elem[B], val mB: Manifest[B])
    extends StagedArrayBase[B] with ViewArray[A, B] {
    override val elem = eB
    implicit def eA: Elem[A] = iso.eA
    implicit def eB: Elem[B] = iso.eB
    implicit def mB: Manifest[B] = eB.manifest

    def index(i: IntRep) = iso.toStaged(arr(i))
    def toArray = {
      val _a = arr.toArray;
      val f = (i: IntRep) => iso.toStaged(_a(i))
      toExp(ArrayTabulate(length, mkLambda(f)))
    }
    def map[R: Elem](f: Rep[B] => Rep[R]): PA[R] = {
      val len = length
      element[R].tabulate(len)(i => f(iso.toStaged(arr(i))))
    }

    def slice(start: IntRep, len: IntRep) = ExpViewArray(arr.slice(start, len), iso)

    override def flagCombine(ifFalse: PA[B], flags: PA[Boolean]) = ifFalse.asInstanceOf[ExpViewArray[A, B]] match {
      case ExpViewArray(falseA, iso) => ExpViewArray(arr.flagCombine(falseA, flags), iso)
      case _ => !!!("ExpViewArray expected by was", ifFalse)
    }

    // length(this) + length(ifFalse) == length(flags)
    override def flagMerge(ifFalse: PA[B], flags: PA[Boolean]) = ifFalse.matchType {
      (a: ExpViewArray[A, B]) => ExpViewArray(arr.flagMerge(a.arr, flags), iso)
    }

    // length(this) == length(flags) == (length(A) + length(B))
    override def flagSplit(flags: PA[Boolean]) = {
      val Pair(at, af) = arr.flagSplit(flags)
      Pair(ExpViewArray(at, iso), ExpViewArray(af, iso))
    }
  }

  case class ArrayFromView[A, B](view: PA[B], iso: Iso[A,B])(implicit eA: Elem[A]) extends ExpStubArray[A]

  class IsoOps[A,B](iso: Iso[A,B]) {
    def toFunTo: Rep[A => B] = fun(iso.toStaged)(iso.eA, iso.eB)
    def toFunFrom: Rep[B => A] = fun(iso.fromStaged)(iso.eB, iso.eA)
  }
  implicit def isoToOps[A,B](iso: Iso[A,B]) = new IsoOps(iso)

  //TODO: implement equality for Isos
  def identityIso[A](implicit eA: Elem[A]): Iso[A, A] =
    new StagedIso[A,A] with IdentityIso[A] {
      override def manifest = eA.manifest
      override def zero = eA.zero
    }

  def nestIso[A,B](iso: Iso[A,B]): Iso[PArray[A], PArray[B]] = {
    implicit val elementA = iso.eA
    implicit val elementB = iso.eB
    val ePAB = element[PArray[B]]
    new StagedIso[PArray[A], PArray[B]] {
      override lazy val eB = ePAB
      override def from = (b: PArray[B]) => ???
      override def to = (a: PArray[A]) => ???
      override def fromStaged = (bs: PA[B]) => bs map iso.fromStaged
      override def toStaged = (as: PA[A]) => as map iso.toStaged
      override def manifest = ePAB.manifest
      override def zero = ePAB.zero
    }
  }

  def pairIso[A1,B1,A2,B2](iso1: Iso[A1,B1], iso2: Iso[A2,B2]): Iso[(A1, A2), (B1,B2)] = {
    implicit val eA1 = iso1.eA
    implicit val eA2 = iso2.eA
    implicit val eB1 = iso1.eB
    implicit val eB2 = iso2.eB
    val eBB = element[(B1,B2)]
    new StagedIso[(A1, A2), (B1,B2)] {
      override def from = (b: (B1,B2)) => (iso1.from(b._1), iso2.from(b._2))
      override def to = (a: (A1, A2)) => (iso1.to(a._1), iso2.to(a._2))
      override def fromStaged = (b: Rep[(B1,B2)]) => (iso1.fromStaged(b._1), iso2.fromStaged(b._2))
      override def toStaged = (a: Rep[(A1, A2)]) => (iso1.toStaged(a._1), iso2.toStaged(a._2))
      def manifest = eBB.manifest
      def zero = eBB.zero
    }
  }

  def composeIso[A,B,C](iso2: Iso[B,C], iso1: Iso[A,B]): Iso[A,C] = {
    implicit val eA = iso1.eA
    new StagedIso[A,C] {
      override def from = (c: C) => iso1.from(iso2.from(c))
      override def to = (a: A) => iso2.to(iso1.to(a))
      override def fromStaged = (c: Rep[C]) => iso1.fromStaged(iso2.fromStaged(c))
      override def toStaged = (a: Rep[A]) => iso2.toStaged(iso1.toStaged(a))
      def manifest = iso2.manifest
      def zero = iso2.zero
    }
  }

  object HasViewArg {
    def unapply[T](d: Def[T]): Option[Def[T]] = {
      val args = dep(d)
      args.exists( _ match { case Def(ExpViewArray(_,_)) => true case _ => false }) match {
        case true => Some(d)
        case _ => None
      }
    }
  }

  object View {
    def unapply[T](e: Exp[T]): Option[(ExpViewArray[Any,Any], Elem[Any], Elem[Any])] =
      e match {
        case Def(view@ExpViewArray(_,_)) => Some((view, view.eA, view.eB))
        case _ => None
      }
  }
  
  def mkViewFrom[A,B](view: ExpViewArray[A,B], arr: PA[A]): PA[B] = {
    implicit val eB = view.eB
    ExpViewArray(arr, view.iso)
  }

  def liftViewFromArgsDefault[T:Elem](d: Def[T]): Rep[_] = {
    val args = dep(d)
    val view = args collect { case View(v, eA, eB) => v } head

    val subst = args collect {
      a => a match { case View(v, eA, eB) => (a.as[Any], v.arr.as[Any]) }} toMap

    val t = new MapTransformer(subst)
    val d1 = mirror(d, t)
    mkViewFrom(view, d1.as[PArray[Any]])
    //ExpViewArray(d1.as[PArray[Any]], view.iso)
  }

  def liftViewFromArgs[T:Elem](d: Def[T]): Rep[_] = d match {
    case ExpNestedArray(Def(view@ExpViewArray(a, iso)), segs) => {
      val nested = mkNestedArray(a, segs)(view.eA)
      val paeA = parrayElement(view.eA)
      implicit val paeB = parrayElement(view.eB)
      ExpViewArray(nested, nestIso(iso)/*(view.eA, view.eB)*/)//(paeA, paeB, paeB.manifest)
    }
    case ReplicateSegPA(c, Def(view@ExpViewArray(arr, iso))) => {
      val eA: Elem[Any] = view.eA
      implicit val eB = view.eB
      ExpViewArray(ReplicateSegPA(c, arr)(eA), iso)//(eA, eB, eB.manifest)
    }
    case NestedArrayValues(Def(view@ExpViewArray(arr, iso))) => {
      val eA: Elem[Any] = view.eA
      implicit val eB = view.eB
      mkViewFrom(view, NestedArrayValues(arr.as[NArray[Any]])(eA))
    }
    case ExpIfArray(_,_,_) => liftViewFromArgsDefault(d)

    case FirstPA(_) => liftViewFromArgsDefault(d)
    case SecondPA(_) => liftViewFromArgsDefault(d)

    case ExpPairArray(
          Def(v1@ExpViewArray(arr1, iso1)),
          Def(v2@ExpViewArray(arr2, iso2))) => {
      val pIso = pairIso(iso1, iso2)
      val arr = ExpPairArray(arr1, arr2)(iso1.eA, iso2.eA)
      implicit val eAB = pIso.eB
      ExpViewArray(arr, pIso)
    }
    case ExpPairArray(Def(v1@ExpViewArray(arr1, iso1)), arr2) => {
      val iso2 = identityIso(arr2.ArrayElem.ea)
      val pIso = pairIso(iso1, iso2)
      val arr = ExpPairArray(arr1, arr2)(iso1.eA, iso2.eA)
      implicit val eAB = pIso.eB
      ExpViewArray(arr, pIso)
    }
    case ExpPairArray(arr2, Def(v1@ExpViewArray(arr1, iso1))) => {
      val iso2 = identityIso(arr2.ArrayElem.ea)
      val pIso = pairIso(iso2, iso1)
      val arr = ExpPairArray(arr2, arr1)(iso2.eA, iso1.eA)
      implicit val eAB = pIso.eB
      ExpViewArray(arr, pIso)
    }
    case _ => ???("Don't know how to lift view from " + d, dep(d))
  }

  override def rewrite[T](d: Def[T])(implicit eT: Elem[T]) = d match {
    case ExpViewArray(Def(ArrayFromView(view, iso2)), iso1) /*if iso1 == iso2*/ => view
    case ArrayFromView(Def(ExpViewArray(arr, iso1)), iso2) /*if iso1 == iso2*/ => arr

    case ExpViewArray(Def(ExpViewArray(arr, iso1)), iso2) => {
      val compIso = composeIso(iso2, iso1)
      implicit val eAB = compIso.eB
      ExpViewArray(arr, compIso)
    }

    case HasViewArg(_) => liftViewFromArgs(d)
    case _ => super.rewrite(d)
  }

}
