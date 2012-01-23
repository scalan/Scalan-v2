package scalan.util

import scalan.common.PimpedType

object Utils {
  trait Matcher[T] extends PimpedType[T] {
    def matchType[T,R](f:T=>R): R = f(value.asInstanceOf[T])
  }
  implicit def toMatcher[T](v: T) = new Matcher[T] { val value = v  }
}

