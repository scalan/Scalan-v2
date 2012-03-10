package scalan.samples

import scalan.common.Common
import Common._
import scalan.sequential.{SequentialContext}
import scalan.staged.StagedContext
import scalan.dsl.{Scalan, ScalanContext}

//trait IsoScalan extends Scalan {
//  type Rep[A] = A
//}
//
//trait IsoContext extends ScalanContext {
//  val scalan: IsoScalan
//}

trait SeqViewSamples { self: ScalanContext =>
  import scalan._

  case class Point(x: Int, y: Int)
  object Point {
    implicit lazy val Zero  = Common.zero(Point(?[Int], ?[Int]))
    class IsoPoint extends IsoBase[(Int, Int), Point] {
      override def from = (p: Point) => (p.x, p.y)
      override def to = (p: (Int, Int)) => Point(p._1, p._2)
      def manifest = Predef.manifest[Point]
      def zero = Zero
    }
    //implicit lazy val isoPoint:Iso[(Int, Int), Point] = new Point.IsoPoint
  }

  case class Circle(loc: Point, r: Int)

  object Circle {
    implicit lazy val Zero = Common.zero(Circle(?[Point], ?[Int]))
    class IsoCircle extends IsoBase[(Point, Int), Circle] {
      override def from = (c: Circle) => (c.loc, c.r)
      override def to = (c: (Point, Int)) => Circle(c._1, c._2)
      def manifest = Predef.manifest[Circle]
      def zero = Zero
    }
    //implicit lazy val isoCircle:Iso[(Point, Int), Circle] = new Circle.IsoCircle
  }

  implicit val isoPoint: Iso[(Int, Int), Point]
  implicit val isoCircle: Iso[(Point, Int), Circle]
}

trait SeqSampleImplicits extends SeqViewSamples { self: SequentialContext =>
  import scalan._

  implicit lazy val isoPoint:Iso[(Int, Int), Point] = new Point.IsoPoint
  implicit lazy val isoCircle:Iso[(Point, Int), Circle] = new Circle.IsoCircle
}

trait StagedViewSamples extends SeqViewSamples { self: StagedContext =>
  import scalan._

  case class ExpPoint(x: Rep[Int], y: Rep[Int]) extends Def[Point]
  trait PointOps {
    def x(implicit e:Elem[Int]): Rep[Int]
    def y(implicit e:Elem[Int]): Rep[Int]
  }
  implicit def repToPointProxyOps(p: Rep[Point]): PointOps = proxyOps[Point, PointOps](p)
  def infix_norma(p: Rep[Point]) = p.x * p.x + (p.y * p.y)

  object ExpPoint {
    class IsoExpPoint extends Point.IsoPoint /*with StagedIso[(Int, Int), Point]*/ {
      override def fromStaged = (p: Rep[Point]) => (p.x, p.y)
      override def toStaged = (p: Rep[(Int, Int)]) => ExpPoint(p._1, p._2)
    }
    addRules({
      case MethodCall(Def(ExpPoint(x,y)), "x", _) => x
      case MethodCall(Def(ExpPoint(x,y)), "y", _) => y
    })
  }
  implicit val isoPoint: Iso[(Int, Int), Point]

  case class ExpCircle(loc: Rep[Point], rad: Rep[Int]) extends Def[Circle]
  trait CircleOps {
    def loc(implicit e:Elem[Point]): Rep[Point]
    def rad(implicit e:Elem[Int]): Rep[Int]
  }
  implicit def repToCircleOps(x: Rep[Circle]): CircleOps = proxyOps[Circle, CircleOps](x)

  object ExpCircle {
    class IsoExpCircle extends Circle.IsoCircle /*with StagedIso[(Point, Int), Circle] */{
      override def fromStaged = (x: Rep[Circle]) => (x.loc, x.rad)
      override def toStaged = (x: Rep[(Point, Int)]) => ExpCircle(x._1, x._2)
    }
    addRules({
      case MethodCall(Def(ExpCircle(loc,_)), "loc", _) => loc
      case MethodCall(Def(ExpCircle(_,rad)), "rad", _) => rad
    })
  }
  implicit val isoCircle: Iso[(Point, Int), Circle]

}

trait StagedSampleImplicits extends StagedViewSamples { self: StagedContext =>
  import scalan._

  implicit lazy val isoPoint: Iso[(Int, Int), Point] = new ExpPoint.IsoExpPoint
  implicit lazy val isoCircle: Iso[(Point, Int), Circle] = new ExpCircle.IsoExpCircle
}
