package scala.virtualization.lms
package internal

import scalan.dsl.ArraysBase
import java.io.{File, PrintWriter, FileOutputStream}

trait GraphVizExport extends Expressions with Scheduling { self: ArraysBase =>

  def quote(x: Any) = "\""+x+"\""
  
  def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = {
    stream.println("label=" + quote(sym + " \\n " + rhs))
    stream.println("shape=box")
  }

  def emitDeps(sym: Sym[_], rhs: Def[_], deps: List[Sym[Any]])(implicit stream: PrintWriter) = {
    for (dep <- deps) {
      stream.println("\"" + dep + "\" -> \"" + sym + "\"")
    }
  }

  def emitDepGraph(d: Def[Any], file: String, landscape: Boolean): Unit =
    emitDepGraph(dep(d), file, landscape)
  def emitDepGraph(start: Exp[Any], file: String, landscape: Boolean = false): Unit =
    emitDepGraph(List(start), file, landscape)
  def emitDepGraph(ss: List[Exp[Any]], file: String, landscape: Boolean): Unit = {
    val f = new File(file)
    f.getParentFile.mkdirs()
    emitDepGraph(ss, new java.io.PrintWriter(new java.io.FileOutputStream(file)), landscape)
  }

  def emitDepGraph(ss: List[Exp[Any]], stream: PrintWriter, landscape: Boolean): Unit = {
    stream.println("digraph G {")

    val deflist = buildScheduleForResult(ss map { (_.asInstanceOf[Sym[Any]]) })

    landscape match {
      case true => stream.println("rankdir=LR")
      case _ =>
    }

    for (TP(sym, rhs) <- deflist) {

      val deps = dep(rhs)

      stream.println(quote(sym) + " [")

      // all

      emitNode(sym, rhs)(stream)

      stream.println("]")
      
      emitDeps(sym, rhs, deps)(stream)

    }

    stream.println("}")
    stream.close()
  }
 
  
  
}
