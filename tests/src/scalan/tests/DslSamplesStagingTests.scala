package scalan.tests

import scalan.samples.DslSamples
import org.junit.Test
import scalan.staged.{ScalanExportGraph, ScalanStaged}

class DslSamplesStagingTests extends DslSamples with FileDiffSuite {
  val prefix = "test-out/DslSamplesStaging/"
  override val scln = new ScalanStaged with ScalanExportGraph { override val isDebug = false }
  import scln._

  @Test def smvmStagingTest = {

    val smvm = mkLambda2[Matrix,Vector,Vector](m => v => matrixVectorMul(m, v))

    withOutFile(prefix+"smvm.txt") {
      emitDepGraph(smvm, prefix + "smvm.dot", false)
    }

    val svm = mkLambda2[SparseVector,Vector,Float](sv => v => sparseVectorMul(sv, v))

    withOutFile(prefix+"svm.txt") {
      emitDepGraph(svm, prefix + "svm.dot", false)
    }
  }
}
