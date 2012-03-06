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

      emitDepGraph(
        List(nested2,
        fun(iso.toStaged)(view.eA, view.eB),
        fun(iso.fromStaged)(view.eB, view.eA)), prefix + name + ".dot", false)
      //emitGraph(nested2, name)
    }
  }

}
