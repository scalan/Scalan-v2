/**
 * Shamelessly taken from https://github.com/namin/lms-sandbox
 */
package scalan.lms.common

import java.lang.{reflect => jreflect}

import java.io.PrintWriter
import scala.virtualization.lms.common.{BaseExp, Base}
import scalan.dsl.ArraysBase

trait ProxyBase extends Base {
  def proxyOps[T,Ops<:AnyRef](x: Rep[T])(implicit m: Manifest[Ops]): Ops
}

trait ProxyExp extends ProxyBase with BaseExp { self: ArraysBase =>

  case class MethodCall[T](receiver: Exp[Any], method: String, args: List[Exp[Any]]) extends Def[T]

  def proxyOps[T,Ops<:AnyRef](x: Rep[T])(implicit m: Manifest[Ops]): Ops = {
    val clazz = m.erasure
    val handler = new InvocationHandler(x)
    val proxy = jreflect.Proxy.newProxyInstance(clazz.getClassLoader(), Array(clazz), handler)
    proxy.asInstanceOf[Ops]
  }

  class InvocationHandler(receiver: Exp[Any]) extends jreflect.InvocationHandler {

    def invoke(proxy: AnyRef, m: jreflect.Method, args: Array[AnyRef]): Exp[Any] = {
//      var clazz = m.getReturnType
//      val manifest = Manifest.classType[AnyRef](clazz)
//      val elem = Element.fromManifest(manifest)

      //TODO: Make a check when constructing proxy, not when executing it. Also, check using
      //reflection by enumerating all methods and checking their signatures
      val args_ = (args == null) match { case true => Array.empty case _ => args }
      val elems = args.seq collect { case e: Elem[_] => e.asInstanceOf[Elem[AnyRef]] }
      elems.length == 0 match {
        case true =>
          !!!("Domain specific operation " + m.getName + " of class " + m.getDeclaringClass.getName +
              " should have implicit Elem[T] argument where T is the return type of the operation")
        case _ =>
      }
      val opArgs = args_ filterNot (_.isInstanceOf[Elem[_]])
      assert(opArgs forall(_.isInstanceOf[Exp[_]]), "At the moment only Exps can be passed as arguments.")

      toExp(MethodCall[AnyRef](
              receiver,
              m.getName,
              opArgs.map(_.asInstanceOf[Exp[Any]]).toList))(elems.last)
    }

  }

}