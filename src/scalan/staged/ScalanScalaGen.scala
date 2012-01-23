package scalan.staged

import virtualization.lms.ppl.ScalaGenScalaOpsPkg
import virtualization.lms.common.{ExportGraph, ScalaGenFunctions, CompileScala}
import java.io.PrintWriter

trait ScalanScalaGen
  extends ScalanStaged
     with CompileScala
     with ScalaGenScalaOpsPkg
     with ScalaGenFunctions
     with ScalaGenStdArray
     with ScalaGenPArrays
{

}

trait ScalanExportGraph extends StagedImplBase with ScalanStaged with ExportGraph {

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = {
    super.emitNode(sym, rhs)
    rhs match {
      case d: PADef[_] => stream.println("color=blue")
      case arr: ArrayDef[_] => stream.println("color=green")
      case l: Lambda[_,_] => stream.println("color=red")
      case _ =>
    }
  }

}
