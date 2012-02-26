package scalan.tests

import scalan.sequential.ScalanSequential
import scalan.common.Common
import Common._
import org.hamcrest._
import core._
import Is._
import org.junit.{Assert, Test}
import Assert._

class SeqViewTests {
  val scln = new ScalanSequential { override val isDebug = false }
  import scln._

  case class Point(x: Int, y: Int)
  object Point {
    implicit val Zero  = Common.zero(Point(?[Int], ?[Int]))
    implicit object isoPoint extends Iso[(Int, Int), Point] {
      def from = (p: Point) => (p.x, p.y)
      def to = (p: (Int, Int)) => Point(p._1, p._2)
      def manifest = Predef.manifest[Point]
      def zero = Zero
    }
  }

  @Test def pointTests() {

    val points = replicate(2, Point(10, 10))
    assertThat(points.length, is(2))

    val zipped = points zip points
    assertThat(zipped.length, is(2))

    val dp = points map { case Point(x, y) => Point(x + 1, y + 1)}
    assertThat(dp.length, is(2))

    val nested = replicate(2, points)
    val values = concat(nested)
    assertThat(values.length, is(4))

    val nested2 = replicate(2, nested)
    assertThat(concat(concat(nested2)).length, is(8))
  }

  case class Circle(loc: Point, r: Int)

  object Circle {
    implicit val Zero = Common.zero(Circle(?[Point], ?[Int]))
    implicit object isoPoint extends Iso[(Point, Int), Circle] {
      def from = (c: Circle) => (c.loc, c.r)
      def to = (c: (Point, Int)) => Circle(c._1, c._2)
      def manifest = Predef.manifest[Circle]
      def zero = Zero
    }
  }

  @Test def circleTests() {
    val arr = replicate(2, Circle(Point(10, 10), 1))
    assertThat(arr.length, is(2))

  }

}
