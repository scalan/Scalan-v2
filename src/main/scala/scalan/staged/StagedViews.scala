package scalan.staged

import scalan.common.Zero
import scalan.dsl.PViews
import scalan.util.Utils._
import scala.virtualization.lms.internal.Expressions

trait StagedViews extends PViews
                  with StagedImplBase
                  with Expressions { self: StagedImplementation =>

  type Iso[A,B] = StagedIso[A,B]
  trait StagedIso[A,B] extends IsoBase[A,B] {
    def fromStaged: Rep[B] => Rep[A]
    def toStaged: Rep[A] => Rep[B]
  }

  override implicit def viewElement[A, B](implicit iso: Iso[A, B], ea: Elem[A]): Elem[B] =
    new StagedElement[B] {
      implicit val elemB = this
      implicit private lazy val m: Manifest[B] = iso.manifest
      private lazy val z = iso.zero

      def manifest: Manifest[B] = m
      def zero = z

      def replicate(count: IntRep, v: Rep[B]) =
        ExpViewArray(ea.replicate(count, iso.fromStaged(v)), iso)

      def replicateSeg(count: IntRep, v: PA[B]) = v match {
        case Def(ExpViewArray(arr, iso1))
             if iso1 == iso => ExpViewArray(ea.replicateSeg(count, arr.asInstanceOf[PA[A]]), iso)
        case _ =>
          ExpViewArray(ea.replicateSeg(count, ArrayFromView(v, iso)), iso)
      }

      def tabulate(len: IntRep)(f: IntRep => Rep[B]) = {
        val arr = ea.tabulate(len)(iso.fromStaged compose f)
        ExpViewArray(arr, iso)
      }

      def tabulateSeg(len: IntRep)(f: IntRep => PA[B]) = {
        val fa = (i: IntRep) => {
          val segB = f(i); segB.asInstanceOf[ViewArray[A, B]].arr
        }
        val arr = ea.tabulateSeg(len)(fa)
        ExpViewArray(arr, iso)
      }

      def empty = ExpViewArray(ea.empty, iso)
      def toRep(p: B) = iso.toStaged(ea.toRep(iso.from(p)))
    }

  case class ExpViewArray[A, B]
    (arr: PA[A], iso: Iso[A, B])
    (implicit val eA: Elem[A], val eB: Elem[B], val mB: Manifest[B])
    extends StagedArrayBase[B] with ViewArray[A, B] {
    override val elem = eB

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

  //TODO: implement equality for nested Iso
  def nestIso[A,B](iso: Iso[A,B])(implicit eA: Elem[A], eB: Elem[B]): Iso[PArray[A], PArray[B]] =
    new StagedIso[PArray[A], PArray[B]] {
      private lazy val paeB = element[PArray[B]]
      def from = (b: PArray[B]) => ???
      def to = (a: PArray[A]) => ???
      def manifest = paeB.manifest
      def zero = paeB.zero
      def fromStaged = (bs: PA[B]) => bs map iso.fromStaged
      def toStaged = (as: PA[A]) => as map iso.toStaged
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
    val eA: Elem[A] = view.eA
    implicit val eB = view.eB
    ExpViewArray(arr, view.iso)(eA, eB, eB.manifest)
  }

  def liftViewFromArgsDefault[T:Elem](d: Def[T]): Rep[_] = {
    val args = dep(d)
    val view = args collect { case View(v, eA, eB) => v } head
    val subst = args collect {
      a => a match { case View(v, eA, eB) => (a.as[Any], v.arr.as[Any]) }} toMap

    val t = new MapTransformer(subst)
    val d1 = mirror(d, t)
    mkViewFrom(view, d1.as[PArray[Any]])
  }

  def liftViewFromArgs[T:Elem](d: Def[T]): Rep[_] = d match {
    case ExpNestedArray(Def(view@ExpViewArray(a, iso)), segs) => {
      val nested = mkNestedArray(a, segs)(view.eA)
      val paeA = parrayElement(view.eA)
      implicit val paeB = parrayElement(view.eB)
      ExpViewArray(nested, nestIso(iso)(view.eA, view.eB))(paeA, paeB, paeB.manifest)
    }
    case ReplicateSegPA(c, Def(view@ExpViewArray(arr, iso))) => {
      val eA: Elem[Any] = view.eA
      implicit val eB = view.eB
      ExpViewArray(ReplicateSegPA(c, arr)(eA), iso)(eA, eB, eB.manifest)
    }
    case NestedArrayValues(Def(view@ExpViewArray(arr, iso))) => {
      val eA: Elem[Any] = view.eA
      implicit val eB = view.eB
//      ExpViewArray(NestedArrayValues(arr.as[NArray[Any]])(eA), iso)(eA, eB, eB.manifest)
      mkViewFrom(view, NestedArrayValues(arr.as[NArray[Any]])(eA))
    }
    case ExpIfArray(_,_,_) => liftViewFromArgsDefault(d)
    //case ExpIfArray(_, t, e) => mkViewFrom(view)
    case _ => ???("Don't know how to lift view from " + d, dep(d))
  }

  override def rewrite[T](d: Def[T])(implicit eT: Elem[T]) = d match {
    case ExpViewArray(Def(ArrayFromView(view, iso2)), iso1) if iso1 == iso2 => view
    case ArrayFromView(Def(ExpViewArray(arr, iso1)), iso2) if iso1 == iso2 => arr
    case HasViewArg(_) => liftViewFromArgs(d)
    case _ => super.rewrite(d)
  }

}
