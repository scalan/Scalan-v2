package scalan.tests

import org.junit.{Assert, Test}
import org.junit.Assert._
import org.hamcrest._
import core._
import Is._
import scalan.dsl._
import scalan.staged._
import scalan.sequential.ScalanSequential
import virtualization.lms.internal.GraphVizExport


class ScalanTests extends FileDiffSuite {

  trait StdArrayOperations extends Scalan {
    def stdArrayOperations(): Rep[Boolean] = {
      val arr = Array(1,2,3)
      val parr = fromArray(toRep(arr))
      parr.length == toRep(3) && !(parr.length == toRep(3))
    }
  }

  @Test def stdArrayOperationsTest = {
    object Seq extends StdArrayOperations with ScalanSequential
    val res = Seq.stdArrayOperations()
    assertThat(res, is(false))

    object Staged extends StdArrayOperations with ScalanStaged
    val res2 = Staged.stdArrayOperations()
    print(res2.toString)
  }

  val prefix = "test-out/ScalanTests/"

  @Test def stdArrayOps = {
    withOutFile(prefix+"stdArrayOps.txt") {
      val o = new StdArrayOperations with ScalanStaged with GraphVizExport
      case class Result[A](x: o.Rep[A]) extends o.Def[A]
      val r = o.stdArrayOperations()
      println(o.globalDefs.mkString("\n"))
      println(r)
      o.emitDepGraph(o.toExp(Result(r))(o.boolElement), prefix + "stdArrayOps.dot", true)
    }
    //assertFileEqualsCheck(prefix+"fft1")
    //assertFileEqualsCheck(prefix+"fft1-dot")
  }

}
