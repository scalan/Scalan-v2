package scalan.sequential

import scalan.dsl.PViews
import scalan.util.Utils._
//import Utils._

trait SeqViews extends PViews { self: SeqImplementation =>

  //type Iso[A,B] = IsoBase[A,B]
  //trait SeqIso[A,B] extends IsoBase[A,B]

  override implicit def viewElement[A,B](implicit iso: Iso[A,B]): Elem[B] =
    new SeqElement[B] {
      implicit val elemB = this
      implicit private def eA = iso.eA
      implicit private def eB = iso.eB
      private lazy val m: Manifest[B] = iso.manifest
      private lazy val z = iso.zero
      def manifest: Manifest[B] = m
      def zero = z

      def replicate(count: IntRep, v: Rep[B]) =
        SeqViewArray(eA.replicate(count, iso.from(v)), iso)

      def replicateSeg(count: IntRep, v: PA[B]) = {
        val va = v.asInstanceOf[ViewArray[A,B]]
        SeqViewArray(eA.replicateSeg(count, va.arr), iso)
      }

      def tabulate(len: IntRep)(f:IntRep => B) = {
        val arr = eA.tabulate(len)(iso.from compose f)
        SeqViewArray(arr, iso)
      }
      def tabulateSeg(len: IntRep)(f:IntRep => PA[B]) = {
        val fa = (i:IntRep) => { val segB = f(i); segB.asInstanceOf[ViewArray[A,B]].arr }
        val arr = eA.tabulateSeg(len)(fa)
        SeqViewArray(arr, iso)
      }
      def empty = SeqViewArray(eA.empty, iso)
    }

  case class SeqViewArray[A, B](arr: PArray[A], iso: Iso[A,B])(implicit eA: Elem[A], eB:Elem[B])
    extends ViewArray[A,B] with SeqPArray[B]
  {
    override val elem = eB

    def index(i: IntRep) = iso.to(arr(i))
    def map[R:Elem](f: B => R): PA[R] = {
      val len = length
      element[R].tabulate(len)(i => f(iso.to(arr(i))))
    }

    def slice(start: IntRep, len: IntRep) = SeqViewArray(arr.slice(start, len), iso)

    override def flagCombine(ifFalse: PA[B], flags: PA[Boolean]) = ifFalse.asInstanceOf[SeqViewArray[A,B]] match {
      case SeqViewArray(falseA, iso) => SeqViewArray(arr.flagCombine(falseA, flags), iso)
      case _ => sys.error("SeqPairArray expected by was" + ifFalse)
    }

    // length(this) + length(ifFalse) == length(flags)
    def flagMerge(ifFalse: PA[B], flags: PA[Boolean]) = ifFalse.matchType {
      (a: SeqViewArray[A,B]) => SeqViewArray(arr.flagMerge(a.arr, flags), iso)
    }

    def flagSplit  (flags: PA[Boolean]) = { // length(this) == length(flags) == (length(A) + length(B))
      val (at,af) = arr.flagSplit(flags)
      (SeqViewArray(at,iso), SeqViewArray(af,iso))
    }
  }
}
