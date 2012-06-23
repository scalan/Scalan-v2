package scalan.staged

import scalan.common.{Zero, Common}
import Common._
import scalan.dsl._
import scalan.lms.common.ProxyExp
import scalan.sequential.SeqImplementation
import virtualization.lms.common.BaseExp
import scala.{Left => L, Right => R}

trait BaseOption extends ArraysBase with PViews {

  object Opt {
    class IsoOption[A:Elem] extends IsoBase[(Unit|A), Option[A]] {
      override def from = (o: Option[A]) => o match { case Some(a) => R(a) case _ => L(()) }
      override def to = (x: (Unit|A)) => x match { case L(_) => None case R(a) => Some(a) }
      def manifest = { implicit val mA = element[A].manifest; Predef.manifest[Option[A]] }
      def zero = Zero.OptionZero[A]
    }
  }
  implicit def isoOption[A:Elem]: Iso[(Unit|A), Option[A]]
}

trait SeqOption extends BaseOption { self: SeqImplementation =>
  implicit def isoOption[A:Elem]: Iso[(Unit|A), Option[A]] = new Opt.IsoOption[A]
}

trait StagedOption extends BaseExp
                      with ArraysBase 
                      with PViews
                      with ProxyExp
                      with BaseOption { self: StagedImplementation =>

  abstract class ExpOption[A:Elem] extends Def[Option[A]]
  case class ExpSome[A:Elem](x: Rep[A]) extends ExpOption[A]
  case class ExpNone[A:Elem]() extends ExpOption[A]

  trait OptionOps[A] {
    def isEmpty(implicit e:Elem[Boolean]): Rep[Boolean]
    def get(implicit e:Elem[A]): Rep[A]
    def getOrElse[B >: A](default: Rep[B])(implicit eB: Elem[B]): Rep[B]
  }
  implicit def repToOptionProxyOps[A:Elem](p: Rep[Option[A]]): OptionOps[A] = {
    implicit val mA = element[A].manifest;
    proxyOps[Option[A], OptionOps[A]](p)
  }

  def infix_toRight[L:Elem,R:Elem](p: Rep[Option[R]], left: => L): Rep[(L|R)] =
    if (p.isEmpty) Left[L,R](left) else Right[L,R](p.get)

  object ExpOption {
    class IsoExpOption[A:Elem] extends Opt.IsoOption[A] {
      override def fromStaged = (p: Rep[Option[A]]) => p.toRight(())
      override def toStaged = (p: Rep[(Unit|A)]) => p.fold(u => ExpNone[A](), a => ExpSome[A](a))
    }
    addRules({
      case MethodCall(Def(ExpSome(x)), "get", _) => x
      case MethodCall(Def(ExpSome(_)), "isEmpty", _) => false
      case MethodCall(Def(ExpNone()), "isEmpty", _) => true
      case MethodCall(Def(ExpSome(x)), "getOrElse", _) => x
      case MethodCall(Def(ExpNone()), "getOrElse", default :: _) => default
    })
  }

  implicit def isoOption[A:Elem]: Iso[(Unit|A), Option[A]] = new ExpOption.IsoExpOption[A]
}








