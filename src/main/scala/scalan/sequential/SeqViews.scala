package scalan.sequential

import scalan.dsl.PViews
import scalan.util.Utils._
//import Utils._

trait SeqViews extends PViews { self: SeqImplementation =>

  override implicit def viewElement[A,B](implicit iso: Iso[A,B], ea: Elem[A]): Elem[B] =
    new SeqElement[B] {
      implicit val elemB = this
      private lazy val m: Manifest[B] = iso.manifest
      private lazy val z = iso.zero

      def manifest: Manifest[B] = m
      def zero = z

      def replicate(count: IntRep, v: Rep[B]) =
        SeqViewArray(ea.replicate(count, iso.from(v)), iso)

      def replicateSeg(count: IntRep, v: PA[B]) = {
        val va = v.asInstanceOf[ViewArray[A,B]]
        SeqViewArray(ea.replicateSeg(count, va.a), iso)
      }

      def tabulate(len: IntRep)(f:IntRep => B) = {
        val arr = ea.tabulate(len)(iso.from compose f)
        SeqViewArray(arr, iso)
      }
      def tabulateSeg(len: IntRep)(f:IntRep => PA[B]) = {
        val fa = (i:IntRep) => { val segB = f(i); segB.asInstanceOf[ViewArray[A,B]].a }
        val arr = ea.tabulateSeg(len)(fa)
        SeqViewArray(arr, iso)
      }
      def empty = SeqViewArray(ea.empty, iso)
    }

  case class SeqViewArray[A, B](a: PArray[A], iso: Iso[A,B])(implicit eA: Elem[A], eB:Elem[B])
    extends ViewArray[A,B] with SeqPArray[B]
  {
    override val elem = eB

    def map[R:Elem](f: B => R): PA[R] = {
      val len = length
      element[R].tabulate(len)(i => f(iso.to(a(i))))
    }

    def slice(start: IntRep, len: IntRep) = SeqViewArray(a.slice(start, len), iso)

    override def flagCombine(ifFalse: PA[B], flags: PA[Boolean]) = ifFalse.asInstanceOf[SeqViewArray[A,B]] match {
      case SeqViewArray(falseA, iso) => SeqViewArray(a.flagCombine(falseA, flags), iso)
      case _ => sys.error("SeqPairArray expected by was" + ifFalse)
    }

    // length(this) + length(ifFalse) == length(flags)
    def flagMerge(ifFalse: PA[B], flags: PA[Boolean]) = ifFalse.matchType {
      (arr: SeqViewArray[A,B]) => SeqViewArray(a.flagMerge(arr.a, flags), iso)
    }

    def flagSplit  (flags: PA[Boolean]) = { // length(this) == length(flags) == (length(A) + length(B))
      val (at,af) = a.flagSplit(flags)
      (SeqViewArray(at,iso), SeqViewArray(af,iso))
    }
  }
}
