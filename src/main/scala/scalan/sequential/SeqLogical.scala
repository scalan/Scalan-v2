package scalan.sequential

import scalan.dsl._

trait SeqLogical extends ScalanLogical {  self: SeqImplBase =>
  def infix_&&(x: Rep[Boolean], y: Rep[Boolean]): Rep[Boolean] = x && y
  def infix_||(x: Rep[Boolean], y: Rep[Boolean]): Rep[Boolean] = x || y
  def unary_not(x: Rep[Boolean]): Rep[Boolean] = !x
}















