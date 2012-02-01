package scala.virtualization.lms
package internal

import java.io.PrintWriter
import scalan.dsl.ArraysBase


trait ScalaCodegen extends GenericCodegen { self: ArraysBase =>

  def emitScalaSource[A,B](f: Exp[A] => Exp[B], className: String, stream: PrintWriter)(implicit eA: Elem[A], eB: Elem[B]): Unit = {

    val x = fresh[A]
    val y = f(x)

    val sA = eA.manifest.toString
    val sB = eB.manifest.toString

    stream.println("/*****************************************\n"+
                   "  Emitting Generated Code                  \n"+
                   "*******************************************/")
    stream.println("class "+className+" extends (("+sA+")=>("+sB+")) {")
    stream.println("def apply("+quote(x)+":"+sA+"): "+sB+" = {")
    
    emitBlock(y)(stream)
    stream.println(quote(getBlockResult(y)))
    
    stream.println("}")
    stream.println("}")
    stream.println("/*****************************************\n"+
                   "  End of Generated Code                  \n"+
                   "*******************************************/")

    stream.flush
  }


  def emitValDef(sym: Sym[_], rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println("val " + quote(sym) + " = " + rhs)
  }
  def emitVarDef(sym: Sym[_], rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println("var " + quote(sym) + " = " + rhs)
  }
  def emitAssignment(lhs: String, rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println(lhs + " = " + rhs)
  }

}

trait ScalaNestedCodegen extends GenericNestedCodegen with ScalaCodegen { self: ArraysBase =>
  
  override def emitScalaSource[A,B](f: Exp[A] => Exp[B], className: String, stream: PrintWriter)
      (implicit eA: Elem[A], eB: Elem[B]): Unit = {
    super.emitScalaSource[A,B](x => reifyEffects(f(x)), className, stream)
  }

  override def quote(x: Exp[_]) = x match { // TODO: quirk!
    case Sym(-1) => "_"
    case _ => super.quote(x)
  }
  
}
