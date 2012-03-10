package scalan.tests

import scalan.samples._
import org.hamcrest._
import core._
import Is._
import org.junit.{Assert, Test}
import Assert._
import scalan.sequential.ScalanSequential
import scalan.sequential.SequentialContext

class IsoLiftingTests extends TestContext
  with StagedViewSamples
  with StagedSampleImplicits
{
  import scalan._
  val prefix = "test-out/IsoLiftingTests/"

  @Test def pointTests() {
    val name = "nested2"
    withEmitException(name){
      val points: PA[Point] = replicate(2, Point(10, 10))
      val nested: Rep[NArray[Point]] = replicate(2, points)
      val nested2: PA[NArray[Point]]  = replicate(2, nested)
      val Def(view@ExpViewArray(_, iso)) = nested2

      emitDepGraph(List(nested2,iso.toFunTo, iso.toFunFrom), prefix + name + "_const_nested2.dot", false)
    }
  }

  @Test def circleTests() {
    val name = "circle"
    withEmitException(name){
      val replicated = replicate(2, Circle(Point(10, 20), 30))
      val Def(view0@ExpViewArray(_, iso0)) = replicated
      emitDepGraph(List(replicated, iso0.toFunTo, iso0.toFunFrom), prefix + name + "_const_replicated.dot", false)

      val points: PA[Point] = replicate(2, Point(10, 20))
      val circles = points map { p => ExpCircle(p, 30) }
      val Def(view1@ExpViewArray(_, iso1)) = circles
      emitDepGraph(List(circles, iso1.toFunTo, iso1.toFunFrom), prefix + name + "_const_mapped.dot", false)

      val nested: Rep[NArray[Circle]] = replicate(2, circles)
      val Def(view2@ExpViewArray(_, iso2)) = nested
      emitDepGraph(List(nested, iso2.toFunTo, iso2.toFunFrom), prefix + name + "_const_nested.dot", false)

      val nested3: PA[NArray[Circle]]  = replicate(2, nested)
      val Def(view3@ExpViewArray(_, iso3)) = nested3
      emitDepGraph(List(nested3, iso3.toFunTo, iso3.toFunFrom), prefix + name + "_const_nested2.dot", false)
    }
  }

}
