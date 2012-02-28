package scalan.samples

import scalan.dsl.ScalanContext
import scalan.common.Common
import Common._
import scalan.staged.StagedContext
import scalan.sequential.{SequentialContext}

trait SeqViewSamples { self: ScalanContext =>
  import scalan._

  case class Point(x: Int, y: Int)
  object Point {
    implicit lazy val Zero  = Common.zero(Point(?[Int], ?[Int]))
    class IsoPoint extends IsoBase[(Int, Int), Point] {
      def from = (p: Point) => (p.x, p.y)
      def to = (p: (Int, Int)) => Point(p._1, p._2)
      def manifest = Predef.manifest[Point]
      def zero = Zero
    }
  }

  case class Circle(loc: Point, r: Int)

  object Circle {
    implicit lazy val Zero = Common.zero(Circle(?[Point], ?[Int]))
    class IsoCircle extends IsoBase[(Point, Int), Circle] {
      def from = (c: Circle) => (c.loc, c.r)
      def to = (c: (Point, Int)) => Circle(c._1, c._2)
      def manifest = Predef.manifest[Circle]
      def zero = Zero
    }
  }

}

trait SeqSampleImplicits extends SeqViewSamples { self: SequentialContext =>
  import scalan._

  implicit lazy val isoPoint:Iso[(Int, Int), Point] = new Point.IsoPoint
  implicit lazy val isoCircle:Iso[(Point, Int), Circle] = new Circle.IsoCircle
}

trait StagedViewSamples extends SeqViewSamples { self: StagedContext =>
  import scalan._

  case class ExpPoint(x: Rep[Int], y: Rep[Int]) extends Def[Point]
  case class PointProp(p: Rep[Point], propName: String) extends Def[Int]

  class PointOps(p: Rep[Point]) {
    def x: Rep[Int] = PointProp(p, "x")
    def y: Rep[Int] = PointProp(p, "y")
  }
  implicit def pimpPoint(p: Rep[Point]) = new PointOps(p)

  object ExpPoint {

    class IsoExpPoint extends Point.IsoPoint with StagedIso[(Int, Int), Point] {
      def fromStaged = (p: Rep[Point]) => p match {
        case Def(ExpPoint(x, y)) => (x,y)
        case _ => (p.x, p.y)
      }

      def toStaged = (p: Rep[(Int, Int)]) => p match {
        case Def(Tup(x, y)) => toExp(ExpPoint(x,y))
        case _ => ExpPoint(p._1, p._2)
      }
    }
  }
  implicit val isoPoint: Iso[(Int, Int), Point]
}

trait StagedSampleImplicits extends StagedViewSamples { self: StagedContext =>
  import scalan._

  implicit lazy val isoPoint: Iso[(Int, Int), Point] = new ExpPoint.IsoExpPoint
}
