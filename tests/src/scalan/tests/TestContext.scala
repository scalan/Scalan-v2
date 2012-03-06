package scalan.tests

import scalan.staged.{ScalanExportGraph, ScalanStaged, StagedContext}


trait TestContext extends StagedContext with FileDiffSuite {
  val scalan = new ScalanStaged with ScalanExportGraph
  import scalan._
  val prefix: String

  def emitGraph(x: Rep[_], name: String){
    withOutFile(prefix + name + ".txt") {
      emitDepGraph(x, prefix + name + ".dot", false)
    }
  }

  def withEmitException(fileName: String)(block: => Unit) = {
    try {
      val b = block;
    }
    catch {
      case ex: StagingException[_] =>
        withOutFile(prefix+ fileName + "_Exception.txt") {
          emitDepGraph(ex.syms, prefix + fileName + "_Exception.dot", false)
          Console.println(globalDefs.mkString("\n"))
        }
        throw ex
    }
  }
}
