package scalan.tests

import scalan.common.Common
import scalan.samples._
import Common._
import org.hamcrest._
import core._
import Is._
import org.junit.{Assert, Test}
import Assert._
import scalan.sequential.ScalanSequential
import scalan.sequential.SequentialContext

class SeqViewTests  {
  val samples = new SeqViewSamples with SeqSampleImplicits with SequentialContext {
    lazy val scalan = new ScalanSequential { override val isDebug = false   }
  }
  import samples._
  import scalan._

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


  @Test def circleTests() {
    val arr = replicate(2, Circle(Point(10, 10), 1))
    assertThat(arr.length, is(2))

  }

}
