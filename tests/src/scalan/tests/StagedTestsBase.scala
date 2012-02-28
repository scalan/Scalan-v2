package scalan.tests

import scalan.staged.{ScalanExportGraph, ScalanStaged}


trait StagedTestsBase extends FileDiffSuite {
  val prefix: String

  trait TestScalan extends ScalanStaged with ScalanExportGraph {
    def emitGraph(x: Rep[_], name: String){
      withOutFile(prefix + name + ".txt") {
        emitDepGraph(x, prefix + name + ".dot", false)
      }
    }
  }
}





