package scala.virtualization.lms
package common

import scalan.dsl.ArraysBase
import internal._

/**
 * Typeclass to lift values in arbitrary P[_]
 */
trait Pure[A, P[_]] {
  def pure(a: => A): P[A]
}

/**
 * The Base trait defines the type constructor Rep, which is the higher-kinded type that allows for other DSL types to be
 * polymorphically embedded.
 *
 * @since 0.1 
 */
trait Base extends EmbeddedControls
{

  type Rep[+T]

  // Rep[T] is an instance of Pure for any T
  implicit def toRep[T](x: T)(implicit p: Pure[T, Rep]): Rep[T] = p.pure(x)

  //private[lms] implicit def _unit[T](x: T): Rep[T] = unit(x)

  //private[lms] def unit[T](x: T): Rep[T]
  //private[lms] implicit def constPure[T]: Pure[T, Rep]
  val isDebug: Boolean = false

  trait AsRep {
    def as[T]: Rep[T]
  }
  implicit def asRep(x: Rep[_]): AsRep = new AsRep {
    def as[T]: Rep[T] = x.asInstanceOf[Rep[T]]
  }
}


/**
 * This trait sets the representation to be based on AST Expression nodes.
 *
 * @since 0.1
 */
trait BaseExp extends Base with Expressions with Transforming
{ self: ArraysBase =>

  type Rep[+T] = Exp[T]
  //private[lms] implicit def constPure[T] = new Pure[T, Exp] { def pure(a: => T): Exp[T] = toExp(Const(a)) }
  //private[lms] def unit[T](x: T): Rep[T] = toRep(x)(constPure)
}

trait ScalaGenBase extends ScalaCodegen with BaseExp { self: ArraysBase =>

}

trait EffectExp extends BaseExp with Effects { self: ArraysBase =>
  
}

trait ScalaGenEffect extends ScalaNestedCodegen with ScalaGenBase with EffectExp { self: ArraysBase =>
  
}
