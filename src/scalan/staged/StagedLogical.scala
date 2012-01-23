package scalan.staged

import collection.mutable.HashMap
import scalan.dsl._

trait StagedLogical extends ScalanLogical with ScalanExpressions { self: StagedImplementation =>
  case class And(lhs: Exp[Boolean], rhs: Exp[Boolean]) extends Def[Boolean]
  case class Or (lhs: Exp[Boolean], rhs: Exp[Boolean]) extends Def[Boolean]
  case class Not(lhs: Exp[Boolean]) extends Def[Boolean]

  def infix_&&(x: Exp[Boolean], y: Exp[Boolean]): Exp[Boolean] = And(x, y)
  def infix_||(x: Exp[Boolean], y: Exp[Boolean]): Exp[Boolean] = Or(x, y)
  def unary_not(x: Rep[Boolean]): Rep[Boolean] = Not(x)
}














