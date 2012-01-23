package scalan.staged

import scalan.common._
import Common._
import scalan.dsl._
import virtualization.lms.common._
import text.Document._
import scalan.util.DocumentExtensions._

trait StagedImplBase extends ArraysBase
                        with BaseExp
                        with FunctionsExp
                        with StagedEqual
                        with StagedIfThenElse
                        with ScalanBaseExp
                        with MonoidOpsExp
{ self: StagedImplementation =>

  //type Elem[A] = Element[A]

  trait StagedElement[A] extends Element[A] {

  }

  def __ifThenElse[T](cond: Rep[Boolean], thenp: => PA[T], elsep: => PA[T])
                     (implicit o: Overloaded1, et: Elem[T]): PA[T] = ExpIfArray(cond, thenp, elsep)

  /*
  * Base class for all case classes that represent type-indexed variants of PArray[A] for different A
  * */
  abstract class PADef[A] extends Def[PArray[A]] with PArray[A] {
  }

  abstract class Closure[A, B] extends Def[A => B] {
    type Env
    def env: Exp[Env]
    def clos: Exp[Env => A => B]
    def clol: Rep[PArray[Env] => PArray[A] => PArray[B]]
    def envElem: Elem[Env]
  }

  case class Clo[E,A,B](
    env : Rep[E],
    clos: Rep[E => A => B],
    clol: Rep[PArray[E] => PArray[A] => PArray[B]])
    (implicit ee: Elem[E], ea: Elem[A], eb: Elem[B])
    extends Closure[A, B]
  {
    type Env = E
    def envElem = ee
  }

  trait BinopArray[A] extends PArray[A] {
    def op: BinOp[A]
    def lhs: PA[A]
    def rhs: PA[A]
    def length = lhs.length
    override def toDoc = group("BinopArray("
      :: nest(2,text(op.toString)) :: ", " :: nest(2, lhs.toDoc) :: ", " :/: nest(2, rhs.toDoc) :: ")" :: ED)
  }

  trait StubArray[A] extends PArray[A] {
    def arr: PA[A]
    override def toDoc = group("StubArray(" :: nest(2, text(arr.toString)) :: ")" :: ED)
  }

  def extractClo[E,A,B](t: Rep[A => B]): Clo[E,A,B] = t match {
    case Def(c@Clo(_, _, _)) => c.asInstanceOf[Clo[E,A,B]]
    case _ => ???("Clo[E,A,B] expected:", t)
  }

  override def mkApply[A:Elem,B:Elem](f: Exp[A => B], x: Exp[A]): Exp[B] = f match {
    case Def(Clo(e, fS, _)) => {
      implicit val elemAny = e.Elem
      super.mkApply(super.mkApply(fS, e), x)
    }
    case _ => super.mkApply(f, x)
  }

  def doApply[A,B](lam: Exp[A => B], x: Exp[A])
                  (implicit ea: Elem[A], eb: Elem[B]): Exp[B] = lam match {
    case Def(Clo(e, fS, _)) => {
      implicit val elemAny = e.Elem
      val f = doApply(fS, e)
      doApply(f, x)
    }
    case Def(Lambda(f,_,_)) => f(x)
    case _ => mkApply(lam, x)
  }

  def mkApplyPA[A:Elem,B:Elem](f: PA[A => B], xs: PA[A]): PA[B] = f match {
    case Def(ExpFuncArray(es, _, fL)) => {
      implicit val elemPAAny = es.Elem
      super.mkApply(super.mkApply(fL, es), xs)
    }
    case _ => ApplyPA(f, xs)
  }

  def doApplyPA[A:Elem,B:Elem](fs: PA[A => B], xs: PA[A]): PA[B] = fs match {
    case Def(farr@ExpFuncArray(es, _, fL)) =>
      implicit val envElem = farr.ee
      doApply(doApply(fL, es), xs)
    case _ => mkApplyPA(fs, xs)         // don't know how to apply so just generate graph node
  }

  implicit def fromPA2PArray[A](x: Rep[PArray[A]]): PArray[A] = fromPA2PADef(x)

  def fromPA2PADef[A](x: Rep[PArray[A]]): PADef[A] = x match {
    case Def(d: PADef[_]) => d
    case s: Sym[_] => {
      val paElem = x.Elem.asInstanceOf[PArrayElem[A]]
      implicit val ea = paElem.ea
      VarPA(x)
    }
    case _ => ???("unexpected symbol:", x)
  }
}






