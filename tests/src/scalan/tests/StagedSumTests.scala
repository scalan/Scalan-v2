package scalan.tests

import scalan.samples._
import org.junit.Test
import scala.{Left => L, Right => R}

class StagedSumTests extends TestContext {
  import scalan._
  val prefix = "test-out/StagedSumTests/"

  @Test def fromArrayTests() {
    val name = "fromArray_"
    withEmitException(name) {
      val arr:Array[(Int|Int)] = Array(L(1), L(2), R(3))
      val sumarr = fromArray(arr)

      emitDepGraph(List(sumarr), prefix + name + "const.dot", false)

      val replicated = replicate(2, sumarr)
      emitDepGraph(List(replicated), prefix + name + "replicated.dot", false)
    }
  }

}
//object T extends StagedSumTests
//import T._
//import scalan._