package scalan.staged

import scalan.dsl.ScalanStdArrayOps
import virtualization.lms.ppl.ArrayOpsExp
import virtualization.lms.common.ScalaGenEffect
import java.io.PrintWriter
import virtualization.lms.internal._
import scalan.common.Monoid

trait StagedStdArrayOps extends ScalanStdArrayOps
                           with ScalanExpressions
                           with ArrayOpsExp
{ self: StagedImplementation =>
  implicit def repArrayToStagedStdArrayOps[T](a: Rep[Array[T]]) = new StagedStdArrayOpsCls(a)

  class StagedStdArrayOpsCls[T](a: Rep[Array[T]]){
    def slice(start:Rep[Int], len:Rep[Int]) = ArraySlice(a, start, len)
    def scan(implicit m: Monoid[T]) = ArrayScan(a, m)
  }

  abstract class ArrayDef[T] extends Def[Array[T]]
  //case class ArrayConst[T](x: Rep[Array[T]]) extends ArrayDef[T]
  case class ArraySlice[T](x: Exp[Array[T]], start:Exp[Int], len:Exp[Int]) extends ArrayDef[T]
  case class ArrayTabulate[T](len:Rep[Int], func: Rep[Int=>T]) extends ArrayDef[T]
  case class ArrayFill[T](len:Rep[Int], value: Rep[T]) extends ArrayDef[T]
  case class ArrayFillSeg[T](len:Rep[Int], value: Rep[Array[T]]) extends ArrayDef[T]
  case class ArrayConcat[T](arr: Rep[Array[Array[T]]]) extends ArrayDef[T]
  case class ArrayScan[T](x: Exp[Array[T]], implicit val m: Monoid[T]) extends ArrayDef[T]

  def matchArrayConst(arrSym: Exp[_])
                     (ai: Array[Int] => Exp[_])
                     (af: Array[Float] => Exp[_])
                     (ab: Array[Boolean] => Exp[_])
                     (orElse: => Exp[_]): Exp[_] = arrSym match {
    case Def(arr) => arr match {
      case Const(x) if x.isInstanceOf[Array[Int]] => ai(x.asInstanceOf[Array[Int]])
      case Const(x) if x.isInstanceOf[Array[Float]] => af(x.asInstanceOf[Array[Float]])
      case Const(x) if x.isInstanceOf[Array[Boolean]] => ab(x.asInstanceOf[Array[Boolean]])
      case _ => orElse
    }
    case _ => orElse
  }

  // from SeqBaseElement.replicateSeg
  def tabulate[A:Manifest](len: Int, seg: Array[A]): Array[A] = {
    val vlen = seg.length
    Array.tabulate(len * vlen)(i => seg(i % vlen))
  }

  // from SeqStdArray.scan
  def scan[T](arr: Array[T])(implicit m: Monoid[T], mT: Manifest[T]): Array[T] = {
    var res = new Array[T](arr.length)
    res.length > 0 match {
      case true =>
        res(0) = m.zero
        for (i <- 1 until arr.length) res(i) = m.append(res(i-1),arr(i-1))
      case _ =>
    }
    res
  }
  def sum[T](arr: Array[T])(implicit m: Monoid[T], mT: Manifest[T]): T = {
    val len = arr.length
    (len == 0) match { case true => return m.zero case _ => }
    val last = arr.length - 1
    val scanned = scan(arr)
    m.append(scanned(last), arr(last))
  }
  override def rewrite[T](d: Def[T])(implicit eT: Elem[T]) = d match {
    case ArrayLength(Def(ArrayScan(arr, _))) =>  ArrayLength(arr)
    case ArrayLength(arrSym: Exp[_]) => matchArrayConst(arrSym)
        { x => Const(x.length) }
        { x => Const(x.length) }
        { x => Const(x.length) }
        { super.rewrite(d) }

    case arrDef@ArrayFill(Def(Const(len)), Def(Const(v))) =>
      implicit val m = arrDef.value.Elem.manifest
      Const(Array.fill(len)(v))

    case arrDef@ArrayFillSeg(Def(Const(len)), arrSym: Exp[_]) =>
      matchArrayConst(arrSym)
        { seg => Const(tabulate(len, seg)) }
        { seg => Const(tabulate(len, seg)) }
        { seg => Const(tabulate(len, seg)) }
        { super.rewrite(d) }

    case ArrayScan(arrSym: Exp[_], m) =>
      matchArrayConst(arrSym)
        { x => Const(scan(x)) }
        { x => Const(scan(x)) }
        { x => Const(scan(x)) }
        { super.rewrite(d) }

    case ArraySum(arrSym: Exp[_], m) =>
      matchArrayConst(arrSym)
        { x => Const(sum(x)) }
        { x => Const(sum(x)) }
        { x => Const(sum(x)) }
        { super.rewrite(d) }

    case _ => super.rewrite(d)
  }

}

trait ScalaGenStdArray extends ScalaGenEffect { this: StagedStdArrayOps =>

  abstract override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case ArrayTabulate(len, func) => emitValDef(sym, "Array.tabualte(" + quote(len) + "," + quote(func) +")")
    case ArraySlice(x, start, len) => emitValDef(sym, "" + quote(x) + ".slice(" + quote(start) + "," + quote(len) +")")
    case ArrayScan(x, _) => emitValDef(sym, "Array.scan(" + quote(x) +")")
    case _ => super.emitNode(sym, rhs)
  }
}














