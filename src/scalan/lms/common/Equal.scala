package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.util.OverloadHack
import scalan.dsl.ArraysBase

trait Equal extends Base with OverloadHack { self: ArraysBase =>
  def equals[A:Elem,B:Elem](a: Rep[A], b: Rep[B]) : Rep[Boolean]
  def notequals[A:Elem,B:Elem](a: Rep[A], b: Rep[B]) : Rep[Boolean]
}

trait EqualExp extends Equal with BaseExp {  self: ArraysBase =>
  case class Equal[A,B](a: Exp[A], b: Exp[B]) extends Def[Boolean]
  case class NotEqual[A,B](a: Exp[A], b: Exp[B]) extends Def[Boolean]

  def equals[A:Elem,B:Elem](a: Rep[A], b: Rep[B]): Rep[Boolean] = Equal(a,b)
  def notequals[A:Elem,B:Elem](a: Rep[A], b: Rep[B]): Rep[Boolean] = NotEqual(a,b)

  //FIXME: these methods bacame ambiguous in Seq implementation when declared in Equal
  def __equal[A,B](a: Rep[A], b: Rep[B])(implicit o: Overloaded1, mA: Elem[A], mB: Elem[B]) : Rep[Boolean] = equals(a,b)
  def __equal[A,B](a: Rep[A], b: B)(implicit o: Overloaded2, mA: Elem[A], mB: Elem[B]): Rep[Boolean] = equals(a, toRep(b))
  //def __equal[A,B](a: A, b: Rep[B])(implicit o: Overloaded3, mA: Elem[A], mB: Elem[B]): Rep[Boolean] = equals(toRep(a), b)

  def infix_!=[A,B](a: Rep[A], b: Rep[B])(implicit o: Overloaded1, mA: Elem[A], mB: Elem[B]) : Rep[Boolean] = notequals(a,b)
  def infix_!=[A,B](a: Rep[A], b: B)(implicit o: Overloaded2, mA: Elem[A], mB: Elem[B]) : Rep[Boolean] = notequals(a, toRep(b))
  //def infix_!=[A,B](a: A, b: Rep[B])(implicit o: Overloaded3, mA: Elem[A], mB: Elem[B]) : Rep[Boolean] = notequals(toRep(a), b)

}

trait ScalaGenEqual extends ScalaGenBase with EqualExp { self: ArraysBase =>
  
  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case Equal(a,b) =>  emitValDef(sym, quote(a) + "==" + quote(b))
    case _ => super.emitNode(sym, rhs)
  }
}
