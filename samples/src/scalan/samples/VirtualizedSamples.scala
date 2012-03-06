package scalan.samples

import scalan.staged.StagedContext
import scalan.dsl.ScalanContext
import scalan.common.Common
import Common._

//trait SeqStruct { self: ScalanContext =>
//  import scalan._
//
//  case class Obj[T](fields: Map[String, Rep[_]]) extends Rep[T]
//
//  def __new[T](args: (String, Boolean, Rep[T] => Rep[_])*): Rep[T] =
//    new Obj(args map {
//      case (n, mut, rhs) => (n, rhs(null))} toMap)
//
//  trait Point extends Struct[Rep] { val x: Int; val y: Int }
//  object Point {
//    implicit lazy val Zero  = Common.zero(new Point { val x = ?[Int]; val y = ?[Int] })
//    class IsoPoint extends IsoBase[(Int, Int), Point] {
//      def from = (p: Point) => (p.x, p.y)
//      def to = (p: (Int, Int)) => Point(p._1, p._2)
//      def manifest = Predef.manifest[Point]
//      def zero = Zero
//    }
//  }
//
//  trait Circle extends Struct[Rep] { val loc: Point; val r: Int }
//  object Circle {
//    implicit lazy val Zero = Common.zero(new Circle { val loc = ?[Point]; val r = ?[Int] })
//    class IsoCircle extends IsoBase[(Point, Int), Circle] {
//      def from = (c: Circle) => (c.loc, c.r)
//      def to = (c: (Point, Int)) => Circle(c._1, c._2)
//      def manifest = Predef.manifest[Circle]
//      def zero = Zero
//    }
//  }
//}

//trait StagedStruct { self: StagedContext =>
//  import scalan._
//
//  case class Obj[T](fields: Map[String, Rep[_]]) extends Rep[T]
//
//  def __new[T](args: (String, Boolean, Rep[T] => Rep[_])*): Rep[T] =
//    new Obj(args map {
//      case (n, mut, rhs) => (n, rhs(null))} toMap)
//
//  trait Point extends Struct[Rep] { val x: Int; val y: Int }
//  object Point {
//    implicit lazy val Zero  = Common.zero(new Point { val x = ?[Int]; val y = ?[Int] })
//    class IsoPoint extends IsoBase[(Int, Int), Point] {
//      def from = (p: Point) => (p.x, p.y)
//      def to = (p: (Int, Int)) => Point(p._1, p._2)
//      def manifest = Predef.manifest[Point]
//      def zero = Zero
//    }
//  }
//
//  trait Circle extends Struct[Rep] { val loc: Point; val r: Int }
//  object Circle {
//    implicit lazy val Zero = Common.zero(new Circle { val loc = ?[Point]; val r = ?[Int] })
//    class IsoCircle extends IsoBase[(Point, Int), Circle] {
//      def from = (c: Circle) => (c.loc, c.r)
//      def to = (c: (Point, Int)) => Circle(c._1, c._2)
//      def manifest = Predef.manifest[Circle]
//      def zero = Zero
//    }
//  }
//}

//trait StructSamples extends SeqViewSamples { self: StagedContext =>
//  import scalan._
//
//  case class Obj[T](fields: Map[String, Rep[_]]) extends Def[T]
//
//  def __new[T](args: (String, Boolean, Rep[T] => Rep[_])*): Rep[T] =
//    Obj(args map {
//      case (n, mut, rhs) => (n, rhs(null))} toMap)
//
//  trait DefPoint extends Struct[Rep] {
//    val x: Int
//    val y: Int
//  }
//  case class Select[T, U](tgt: Exp[U], field: String) extends Def[T]
//
//  class PointOps(p: Rep[Point]) {
//    def x: Rep[Int] = p match {
//      case Def(ExpPoint(x,y)) => x
//      case _ => StructProp(p, "x")
//    }
//    def y: Rep[Int] = p match {
//      case Def(ExpPoint(x,y)) => y
//      case _ => StructProp(p, "y")
//    }
//  }
//  implicit def pimpPoint(p: Rep[Point]) = new PointOps(p)
//
//  implicit def selectOps(self: Exp[_ <: Point]) = new {
//    def selectDynamic[T](n: String): Exp[T] = StructProp(self, n)
//  }
//
//  object ExpPoint {
//
//    class IsoExpPoint extends Point.IsoPoint with StagedIso[(Int, Int), Point] {
//      def fromStaged = (p: Rep[Point]) => p match {
//        case Def(ExpPoint(x, y)) => (x,y)
//        case _ => (p.x, p.y)
//      }
//
//      def toStaged = (p: Rep[(Int, Int)]) => p match {
//        case Def(Tup(x, y)) => toExp(ExpPoint(x,y))
//        case _ => ExpPoint(p._1, p._2)
//      }
//    }
//  }
//  implicit val isoPoint: Iso[(Int, Int), Point]
//}
