package scalan.util

import scalan.common.PimpedType

object Utils {
  trait Matcher[T] extends PimpedType[T] {
    def matchType[T,R](f:T=>R): R = f(value.asInstanceOf[T])
  }
  implicit def toMatcher[T](v: T) = new Matcher[T] { val value = v  }
}


object Dissection {

  sealed abstract class Expr
  case class Val(i: Int) extends Expr
  case class Add(a: Expr,  b: Expr) extends Expr

  def eval(e: Expr): Int = e match {
    case Val(i) => i
    case Add(a,b) => eval(a) + eval(b)
  }
  
//  class Evaluator {
//    type Stack = List[Either[Int, Expr]]
//    var stk = List[Either[Int, Expr]]
//    def start() { stk = List() }
//
//    def eval(e: Expr, s: Stack): Stack = e match {
//      case Val(i) => i :: s
//      case Add(a, b) =>
//    }
//  }
}