package scalan.tests

import org.junit.Test
import scalan.samples.{StagedSampleImplicits, StagedViewSamples}


class StagedViewTests extends TestContext
  with StagedViewSamples
  with StagedSampleImplicits
{
  import scalan._
  val prefix = "test-out/StagedViewTests/"

  @Test def pointTests() {
    val points = replicate(2, Point(10, 20))
    emitGraph(points, "points")

    val zipped = points zip points
    emitGraph(zipped, "zipped")

    val dp = points map {
      p => ExpPoint(p.x + 1, p.y + 1)
    }
    emitGraph(dp, "dp")

    val dpFunc = mkLambda((_: PA[Point]) map {
      p => ExpPoint(p.x + 1, p.y + 1)
    })
    emitGraph(dpFunc, "dpFunc")

    val nested = replicate(2, points)
    emitGraph(nested, "nested")

    val values = concat(nested)
    emitGraph(values, "values")

    val nested2 = replicate(2, nested)
    emitGraph(nested2, "nested2")
  }
//
//  case class Circle(loc: Point, r: Int)
//
//  object Circle {
//    implicit val Zero = Common.zero(Circle(?[Point], ?[Int]))
//
//    implicit object isoPoint extends Iso[(Point, Int), Circle] {
//      def from = (c: Circle) => (c.loc, c.r)
//
//      def to = (c: (Point, Int)) => Circle(c._1, c._2)
//
//      def manifest = Predef.manifest[Circle]
//
//      def zero = Zero
//    }
//
//  }
//
//  @Test def circleTests() {
//    val arr = replicate(2, Circle(Point(10, 10), 1))
//    assertThat(arr.length, is(2))
//
//  }

}
