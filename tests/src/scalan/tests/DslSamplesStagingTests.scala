package scalan.tests

import scalan.samples.DslSamples
import org.junit.Test
import scalan.staged.{ScalanExportGraph, ScalanStaged}

class DslSamplesStagingTests extends StagedTestsBase {
  override val prefix = "test-out/DslSamplesStaging/"
  val samples = new DslSamples with TestScalan
  import samples._

  @Test def smvmStagingTest = {

    val dotp = mkLambda({ p: Rep[(Vector, Vector)] =>
      val Pair(v1, v2) = p; dotProduct(v1, v2)})

    withOutFile(prefix+"dotp.txt") {
      emitDepGraph(dotp, prefix + "dotp.dot", false)
    }

    val smvm = mkLambda2[Matrix,Vector,Vector](m => v => matrixVectorMul(m, v))

    withOutFile(prefix+"smvm.txt") {
      emitDepGraph(smvm, prefix + "smvm.dot", false)
    }

    val svm = mkLambda2[SparseVector,Vector,Float](sv => v => sparseVectorMul(sv, v))

    withOutFile(prefix+"svm.txt") {
      emitDepGraph(svm, prefix + "svm.dot", false)
    }

    withOutFile(prefix+"qsort.txt") {
      emitDepGraph(qsort, prefix + "qsort.dot", false)
    }

    withOutFile(prefix+"qsort1.txt") {
      emitDepGraph(qsort1, prefix + "qsort1.dot", false)
    }

    withOutFile(prefix+"qsortV.txt") {
      emitDepGraph(qsortV, prefix + "qsortV.dot", false)
    }
  }
}
