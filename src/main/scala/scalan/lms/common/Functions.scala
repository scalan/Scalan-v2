package scala.virtualization.lms
package common

import java.io.PrintWriter
import scalan.dsl.ArraysBase

//import scalan.staged.StagedImplBase.Clo

trait Functions extends Base { self: ArraysBase =>

  class LambdaOps[A:Elem,B:Elem](f: Rep[A => B]) {
    def apply(x: Rep[A]): Rep[B] = mkApply(f,x)
  }
  implicit def pimpLambda[A:Elem,B:Elem](f: Rep[A=>B]) = new LambdaOps[A,B](f)

  implicit def mkLambda[A,B](fun: Rep[A] => Rep[B])(implicit eA: Elem[A], eB: Elem[B]): Rep[A => B]
  def mkApply[A:Elem,B:Elem](fun: Rep[A => B], arg: Rep[A]): Rep[B]
  def letrec[A,B](f: (Rep[A=>B])=>(Rep[A]=>Rep[B]))(implicit eA: Elem[A], eb:Elem[B]): Rep[A=>B]
  def fun[A,B](f: Rep[A] => Rep[B])(implicit eA: Elem[A], eB: Elem[B]): Rep[A => B] = mkLambda(f)
}

trait FunctionsExp extends Functions with EffectExp { self: ArraysBase =>

  case class Lambda[A,B](f: Exp[A] => Exp[B], x: Sym[A], y: Exp[B])(implicit val eA: Elem[A]) extends Def[A => B] {
    override def hashCode: Int = (41 * (41 + x.hashCode) + y.hashCode)
    override def equals(other: Any) =
      other match {
        case that: Lambda[_,_] =>
          (that canEqual this) &&
          (this.x equals that.x) &&
          (this.y equals that.y)
        case _ => false
      }
    def canEqual(other: Any) = other.isInstanceOf[Lambda[_,_]]
    override def toString = "Lambda(" + x + "," + y + ")"
  }

  case class Apply[A,B](f: Exp[A => B], arg: Exp[A])(implicit val eA: Elem[A], val eB: Elem[B]) extends Def[B]

//  def mkLambda[A,B](f: Exp[A] => Exp[B])(implicit eA: Elem[A], eB: Elem[B]) : Exp[A => B] = {
//    val x = fresh[A]
//    val y = reifyEffects(f(x)) // unfold completely at the definition site.
//                               // TODO: this will not work if f is recursive.
//                               // need to incorporate the other pieces at some point.
//    Lambda(f, x, y)
//  }
  
  def mkApply[A:Elem,B:Elem](f: Exp[A => B], x: Exp[A]): Exp[B] = {
    recursion.find(m => m._3 == f) match {
      case None =>  // not in recursion, so lookup definition
        f match {
//          case Def(Lambda(_,_,Def(Reify(_,_)))) =>  // if function result is known to be effectful, so is application
//            reflectEffect(Apply(f,x))
          case Def(Lambda(g,_,_)) =>  // if function result is known to be pure, so is application
            g(x)
            //Apply(f, x)
          case Def(Apply(_, _)) =>  // function that is a result of Apply (high order value)
            Apply(f, x)
          case _ => // unknown function
            Apply(f, x)
        }
      case Some(_) =>  // f is not in Defs table at this time, thus a special case here
        f.isRecursive = true
        Apply(f, x) // hit recursion call ! so just make an application
    }
  }

  def letrec[A,B](f: (Rep[A=>B])=>(Rep[A]=>Rep[B]))(implicit eA: Elem[A], eb:Elem[B]): Rep[A=>B] = {
    val x = fresh[A]
    val res = fresh[A => B]
//    val tp = TP(res) { // evaluated lazyly
//      val fun = f(res)
//      val y = exec(fun, x, res)
//      val lam = Lambda(fun, x, y)
//      setDefinition(res, lam)
//      lam
//    }
//    globalDefs = globalDefs:::List(tp)
//    res
    val fun = f(res)
    val y = exec(fun, x, res)
    val lam = Lambda(fun, x, y)
    findDefinition(lam) match {
      case Some(TP(sym, Lambda(f, _, _))) => {
        f equals fun match {
          case true => /*nVars -= 1;*/ sym
          case false =>
            createDefinition(res, lam)
            res
        }
      }
      case None =>
        createDefinition(res, lam)
        res
    }
  }

  def mkLambda[A,B](fun: Exp[A] => Exp[B])(implicit eA: Elem[A], eb:Elem[B]) : Exp[A=>B] = {
    letrec[A,B](f => fun)
  }

  def lambda[A,B](x: Rep[A])(fun: Exp[A] => Exp[B])(implicit eA: Elem[A], eb:Elem[B]): Exp[A=>B] = {
    val res = fresh[A => B]
    val y = exec(fun, x, res)
    val lam = Lambda(fun, x.asInstanceOf[Sym[A]], y)
    findDefinition(lam) match {
      case Some(TP(sym, Lambda(f, _, _))) => {
        f equals fun match {
          case true => /*nVars -= 1;*/ sym
          case false =>
            createDefinition(res, lam)
            res
        }
      }
      case None =>
        createDefinition(res, lam)
        res
    }
  }

  def lambda2[A,B,C](x: Rep[A])(fun: Exp[A] => Exp[B] => Exp[C])(implicit eA: Elem[A], eb:Elem[B], ec:Elem[C]): Exp[A=>B=>C] = {
    val y = fresh[B]
    lambda(x)((a: Rep[A]) => lambda(y)((b:Rep[B]) => fun(a)(b)))
  }

  var recursion: List[(Function[_,_], Exp[Any], Exp[Any])] = List()

  def exec[A,B](f: Exp[A]=>Exp[B], x: Exp[A], fSym: Exp[A => B])(implicit eA: Elem[A], eb:Elem[B]): Exp[B] = {
    recursion.find(m => m._1 == f) match {
      case None =>
        val saveRecursion = recursion
        recursion = (f, x, fSym)::recursion
        val res = f(x) // execute looking for recursive call back to this exec
        recursion = saveRecursion
        res
      case Some((_, _, fs)) => // hit recursion call !
        fs.isRecursive = true
        Apply(fs.asInstanceOf[Exp[A=>B]], x)
    }
  }

  def mkLambda2[A,B,C]
              (fun: Rep[A]=>Rep[B]=>Rep[C])
              (implicit eA: Elem[A], eB: Elem[B], eC: Elem[C]): Rep[A=>B=>C] = {
    val y = fresh[B]
    mkLambda((a: Rep[A]) => lambda(y)((b:Rep[B]) => fun(a)(b)))
  }

  def mkLambda3[A,B,C,D]
              (f: Rep[A]=>Rep[B]=>Rep[C]=>Rep[D])
              (implicit eA: Elem[A], eB: Elem[B], eC: Elem[C], eD: Elem[D]): Rep[A=>B=>C=>D] = {
    val y = fresh[B]
    val z = fresh[C]
    mkLambda((a:Rep[A]) => lambda(y)((b: Rep[B]) => lambda(z)(f(a)(b))))
  }
}

trait ScalaGenFunctions extends ScalaGenEffect with FunctionsExp { self: ArraysBase =>
  override def syms(e: Any): List[Sym[Any]] = e match {
    case Lambda(f, x, y) if shallow => Nil // in shallow mode, don't count deps from nested blocks
    case _ => super.syms(e)
  }
  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case e@Lambda(fun, x, y) =>
      stream.println("val " + quote(sym) + " = {" + quote(x) + ": (" + e.eA.manifest + ") => ")
      emitBlock(y)
      stream.println(quote(getBlockResult(y)))
      stream.println("}")
    case Apply(fun, arg) => 
      emitValDef(sym, quote(fun) + "(" + quote(arg) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}
