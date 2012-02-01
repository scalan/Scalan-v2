package scalan.dsl

import virtualization.lms.common._
import annotation.implicitNotFound
//import scalan.common.Zeros._
import scalan.common.{Monoid, Zero}
import text.Document
import text.Document._

trait ScalanBase extends Base {
  class StagingException[A](message: String, val sym: Rep[A]) extends RuntimeException(message)

  def ???(): Nothing = ???("not implemented")
  def ???(msg: String): Nothing = sys.error(msg)
  def ???[A](msg: String, sym: Rep[A]): Nothing = throw new StagingException(msg + " " + sym.toString, sym)

  def !!! = sys.error("should not be called")
  def !!!(msg: String): Nothing = sys.error(msg)
  def !!![A](msg: String, sym: Rep[A]): Nothing = throw new StagingException(msg + " " + sym.toString, sym)

  object IfThenElseHack {
    def ifThenElse[T](cond: Boolean, thenp: => T, elsep: => T): T = if (cond) thenp else elsep
  }

}










