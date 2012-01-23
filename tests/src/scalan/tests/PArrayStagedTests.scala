package scalan.tests

import org.junit.{Assert, Test}
import org.junit.Assert._
import org.hamcrest._
import core._
import Is._
import scalan.staged.{ScalanStaged, ScalanExportGraph}
import scalan.samples.DslSamples
import scalan.util.DocumentExtensions._

trait PArrayStagedTestBase extends FileDiffSuite with DslSamples {
  val scln = new ScalanStaged with ScalanExportGraph { override val isDebug = false }
  import scln._
  val prefix: String
}

class PArrayStagedTests extends PArrayStagedTestBase {
  val prefix = "test-out/PArrayStagedTests/"
  import scln._

  @Test def dslSamples = {

    val a1 = Array(10f, 20f, 30f)
    val a2 = Array(10f, 10f, 10f)

    val dotP = (arrs: Rep[(Array[Float], Array[Float])]) => {
      val Pair(a, b) = arrs
      val v1 = fromArray(a)
      val v2 = fromArray(b)
      dotProduct(v1, v2)
    }

    val rowinds = Array(0, 1)
    val smvm = (arrs: Rep[(Array[Int], (Int, Float))]) => {
      val Pair(inds, Pair(len, v)) = arrs
      val rowinds = fromArray(inds)
      val rowvals = replicate(len, v)
      val row = rowinds zip(rowvals)
      val matr = replicate(toRep(2),row)
      val vec = replicate(toRep(2), toRep(0.5f))
      matrixVectorMul(matr, vec)
    }

    withOutFile(prefix+"dslSamples.txt") {
      emitDepGraph(vectorOfSquares(10), prefix + "vectorOfSquares.dot", false)
      emitDepGraph(vectorOfSquares(10).toArray, prefix + "vectorOfSquaresToArray.dot", false)
      emitDepGraph(dotP(Pair(a1, a2)), prefix + "dotProduct.dot", false)
      val rowinds2 = fromArray(rowinds)
      emitDepGraph(rowinds2, prefix + "fromArray.dot", false)

      val rowvals = replicate(2, 0.5f)
      emitDepGraph(rowvals, prefix + "replicate.dot", false)

      val row = rowinds2 zip(rowvals)
      emitDepGraph(row, prefix + "zip.dot", false)

      val matr = replicate(2, row)
      emitDepGraph(matr, prefix + "replicateRow.dot", false)

      emitDepGraph(smvm(Pair(rowinds, Pair(toRep(2), toRep(0.5f)))), prefix + "matrixVectorMul.dot", false)

    }

  }



}

