package scalan.samples

import scalan.staged.StagedContext

trait TypeClasses extends StagedViewSamples with StagedSampleImplicits { self: StagedContext =>
  import scalan._
  
  abstract class Monoid[A:Elem] {
    type M = Rep[A]
    def add(x: M, y: M): M
  }
  
  implicit object intRepMonoidSum extends Monoid[Int] {
    def add(x: M, y: M): M = x + y
  }

  object intRepMonoidMul extends Monoid[Int] {
    def add(x: M, y: M): M = x * y
  }

  implicit object pointRepMonoidSum extends Monoid[Point] {
    def add(x: Rep[Point], y: Rep[Point]): Rep[Point] = ExpPoint(x.x + y.x, x.y + y.y)
  }

  def vectSum[A:Monoid:Elem](xs: PA[A], ys: PA[A]): PA[A] =
    (xs zip ys) map { case Pair(x,y) => implicitly[Monoid[A]].add(x, y) }
  
}
