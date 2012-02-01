package scala.virtualization.lms
package internal

import scalan.dsl.ArraysBase

trait Effects extends Expressions { self: ArraysBase =>
  
  type State = List[Exp[_]]
  
  var context: State = _
  
  def reflectEffect[A](x: Def[A])(implicit ea: Elem[A]): Exp[A] = {
     // don't do CSE on effectful computations
    val r: Exp[A] = createDefinition(fresh[A], Reflect(x, context)).sym
    context :+= r
    r
  }
  
  def reifyEffects[A](block: => Exp[A])(implicit ea: Elem[A]): Exp[A] = {
    val save = context
    context = Nil
    
    val result = block
    val resultR = context.isEmpty match {
      case true => result
      case _ =>  Reify(result, context): Exp[A]
    }
    context = save
    resultR
  }

  case class Reflect[A](x:Def[A], effects: List[Exp[Any]]) extends Def[A]
  case class Reify[A](x: Exp[A], effects: List[Exp[Any]]) extends Def[A]
}
