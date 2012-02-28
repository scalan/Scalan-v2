package scalan.staged

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

      def replicateSeg(count: IntRep, v: PA[B]) = {
        val va = v.asInstanceOf[ViewArray[A, B]]
        ExpViewArray(ea.replicateSeg(count, va.a), iso)
      }

      def tabulate(len: IntRep)(f: IntRep => Rep[B]) = {
        val arr = ea.tabulate(len)(iso.fromStaged compose f)
        ExpViewArray(arr, iso)
      }

      def tabulateSeg(len: IntRep)(f: IntRep => PA[B]) = {
        val fa = (i: IntRep) => {
          val segB = f(i); segB.asInstanceOf[ViewArray[A, B]].a
        }
        val arr = ea.tabulateSeg(len)(fa)
        ExpViewArray(arr, iso)
      }

      def empty = ExpViewArray(ea.empty, iso)
      def toRep(p: B) = iso.toStaged(ea.toRep(iso.from(p)))
    }

  case class ExpViewArray[A, B]
    (a: PA[A], iso: Iso[A, B])
    (implicit eA: Elem[A], eB: Elem[B], mB: Manifest[B])
    extends StagedArrayBase[B] with ViewArray[A, B] {
    override val elem = eB

    def index(i: IntRep) = iso.toStaged(a(i))
    def toArray = {
      val _a = a.toArray;
      val f = (i: IntRep) => iso.toStaged(_a(i))
      toExp(ArrayTabulate(length, mkLambda(f)))
    }
    def map[R: Elem](f: Rep[B] => Rep[R]): PA[R] = {
      val len = length
      element[R].tabulate(len)(i => f(iso.toStaged(a(i))))
    }

    def slice(start: IntRep, len: IntRep) = ExpViewArray(a.slice(start, len), iso)

    override def flagCombine(ifFalse: PA[B], flags: PA[Boolean]) = ifFalse.asInstanceOf[ExpViewArray[A, B]] match {
      case ExpViewArray(falseA, iso) => ExpViewArray(a.flagCombine(falseA, flags), iso)
      case _ => !!!("ExpViewArray expected by was", ifFalse)
    }

    // length(this) + length(ifFalse) == length(flags)
    override def flagMerge(ifFalse: PA[B], flags: PA[Boolean]) = ifFalse.matchType {
      (arr: ExpViewArray[A, B]) => ExpViewArray(a.flagMerge(arr.a, flags), iso)
    }

    // length(this) == length(flags) == (length(A) + length(B))
    override def flagSplit(flags: PA[Boolean]) = {
      val Pair(at, af) = a.flagSplit(flags)
      Pair(ExpViewArray(at, iso), ExpViewArray(af, iso))
    }
  }

}
