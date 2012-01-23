package scala.virtualization.lms
package common

import internal.ScalaCompile
import scalan.dsl.ArraysBase

trait Compile extends Base { self: ArraysBase =>
  
  def compile[A,B](f: Rep[A] => Rep[B])(implicit eA: Elem[A], eB: Elem[B]): A=>B
  
}

trait CompileScala extends Compile with ScalaCompile { this: ScalaGenBase with ArraysBase =>
  
}
