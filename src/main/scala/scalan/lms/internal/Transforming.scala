package scala.virtualization.lms.internal

import scala.virtualization.lms.util.OverloadHack
import scalan.dsl.ArraysBase
import reflect.SourceContext
import collection.mutable.HashMap

trait Transforming extends Expressions with OverloadHack {
  self: ArraysBase =>

  abstract class Transformer { // a polymorphic function, basically...
    def apply[A](x: Exp[A]): Exp[A]
    def apply[A](xs: List[Exp[A]]): List[Exp[A]] = xs map (e => apply(e))
    def apply[A](xs: Seq[Exp[A]]): Seq[Exp[A]] = xs map (e => apply(e))
    def apply[X,A](f: X=>Exp[A]): X=>Exp[A] = (z:X) => apply(f(z))
    def apply[X,Y,A](f: (X,Y)=>Exp[A]): (X,Y)=>Exp[A] = (z1:X,z2:Y) => apply(f(z1,z2))
    def onlySyms[A](xs: List[Sym[A]]): List[Sym[A]] = xs map (e => apply(e)) collect { case e: Sym[A] => e }
  }

  object IdentityTransformer extends Transformer {
    def apply[A](x: Exp[A]) = x
  }

  def mirror[A](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[_] = !!!("don't know how to mirror " + e)

  class SubstTransformer extends Transformer {
    val subst = new HashMap[Exp[Any], Exp[Any]]
    def apply[A](x: Exp[A]): Exp[A] = subst.get(x) match {
      case Some(y) if y != x => apply(y.asInstanceOf[Exp[A]])  // transitive closure
      case _ => x
    }
    def +=(kv: (Exp[Any], Exp[Any])) = subst += kv
  }

  class MapTransformer(val subst: Map[Exp[Any], Exp[Any]])
    extends Transformer {

    def apply[A](x: Exp[A]): Exp[A] = subst.get(x) match {
      case Some(y) if y != x => apply(y.asInstanceOf[Exp[A]])  // transitive closure
      case _ => x
    }
  }

}
