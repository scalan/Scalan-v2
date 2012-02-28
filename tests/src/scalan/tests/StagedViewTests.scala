package scalan.tests

import scalan.common.Common
import Common._
import org.hamcrest._
import core._
import Is._
import org.junit.{Assert, Test}
import Assert._
import scalan.samples.{StagedSampleImplicits, StagedViewSamples}
import scalan.staged.{ScalanStaged, StagedContext, ScalanExportGraph}

class StagedViewTests extends FileDiffSuite {//extends StagedTestsBase {
  val samples = new StagedViewSamples with StagedSampleImplicits with StagedContext {
    lazy val scalan = new ScalanStaged with ScalanExportGraph { override val isDebug = false   }
  }
  import samples._
  import scalan._

  val prefix = "test-out/StagedViewTests/"

  def emitGraph(x: Rep[_], name: String){
    withOutFile(prefix + name + ".txt") {
      emitDepGraph(x, prefix + name + ".dot", false)
    }
  }

  @Test def pointTests() {

    val points = replicate(2, Point(10, 10))
    emitGraph(points, "points")

    val zipped = points zip points
    emitGraph(zipped, "zipped")

    val dp = points map {
      p => ExpPoint(p.x + 1, p.y + 1)
    }
    emitGraph(dp, "dp")

    val nested = replicate(2, points)
    emitGraph(nested, "nested")

    val values = concat(nested)
    emitGraph(values, "values")

    val nested2 = replicate(2, nested)
    emitGraph(nested2, "nested")
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
