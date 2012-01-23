package scalan.staged
import scala.collection._

//class StagedTransform {  self: StagedImplementation =>
//
//  type Subst = Map[Rep[_], Rep[_]]
//  type Context = Scope
//
//  class SubstOps(s: Subst) {
//    //def /~(x: Rep[_]): Rep[_] = Scope.applySubst(s, x)
//  }
//  implicit def pimpSubst(s: Subst) = new SubstOps(s)
//
//  class Scope(enclosing: Option[Scope], lam: Option[Rep[_]], s: Subst) {
//    lazy val freeVars = lam ? getFreeVars(_:Rep[_]) | immutable.Set.empty[Rep[_]]
//
//    private val mapping = mutable.Map[Rep[_], Rep[_]]() ++ s
//
//    def apply(t: Rep[_]): Rep[_] = getRecursive(t)
//
//    def applySubst(key: Rep[_]): Option[Rep[_]] =  mapping.get(key) match {
//      case r@Some(_) => r
//      case None => enclosing flatMap {_.applySubst(key)}
//    }
//
//    def add(key: Rep[_], value: Rep[_]) = {
//      mapping += ((key, value))
//    }
//
//    def getRecursive(x: Rep[_]): Rep[_] = applySubst(x) match {
//      case Some(y) if y != x => getRecursive(y)
//      case Some(y) if y == x =>
//        !!!("invalid substitution in " + this + " for ", x)
//      case _ => x
//    }
//
//    def get(x: Rep[_]): Option[Rep[_]] = {
//      val y = getRecursive(x)
//      x equals y match {
//        case true => None
//        case _ => Some(y)
//      }
//    }
//  }
//
//  object Scope {
//    def apply() = new Scope(None, None, Map())
//
//  }
//
//  object Transformer {
//    private var memoizedDefs = Map.empty[Sym[_], Sym[_]]
//    implicit def lifter2Elem[A](implicit l: Transformer[A]): Elem[A] = l.elem
//    implicit def elem2Manifest[A](implicit ea: Elem[A]): Manifest[A] = ea.manifest
//    type PAFunc[A,B,C] = Rep[PArray[A] => PArray[B] => PArray[C]]
//
//    def mkClosure[A,B]
//                 (ctx: Context, lam: Rep[A => B])
//                 (implicit ea: Elem[A], eb: Elem[B]): Closure[A,B] =
//    {
//      val Def(l@Lambda(_, x, body)) = lam
//      val fvars = getFreeVars(lam) map { ctx(_) } toList   // map free vars to this context
//      val env: Rep[_] = mkTuple(fvars)
//      val schedule = scheduleBody(lam)
//
//      implicit val elemAny: Elem[Any] = env.Elem.getElem
//      val clos = mkLambda2((e1: Rep[_]) => (x1: Rep[A]) => {
//        val es = mkTupleExtraction(e1)
//        var sub = ((fvars zip es) filterNot { case (v,e) => v equals e }).toMap
//        x equals x1 match {
//          case false => sub ++= Map(x -> x1)
//          case _ =>
//        }
//        val ctx1 = new Scope(Some(ctx), Some(lam), sub)
//        for (d <- schedule) { doVectorize(ctx1, d) }
//        ctx1(body).asInstanceOf[Rep[B]]
//      })
//      (elemAny, ea, eb)
//
//      implicit val elemPAAny: Elem[PArray[Any]] = parrayElement(elemAny)
//      val clol = mkLambda2((e1:PA[Any]) => (x1: PA[A]) => {
//        val es = mkPATupleExtraction(e1)
//        var sub = ((fvars zip es) filterNot { case (v,e) => v equals e }).toMap
//        x equals x1 match {
//          case false => sub ++= Map(x -> x1)
//          case _ =>
//        }
//        val ctx1 = new Scope(Some(ctx), Some(lam), sub)
//        for (d <- schedule) { doLift(ctx1, d, e1.length) }
//        ctx1(body).asInstanceOf[PA[B]]
//      })
//
//      Clo[Any, A, B](env, clos, clol)(env.Elem.getElem, ea, element[B])
//    }
//
////    def mkClosurePA[A,B]
////                 (ctx: Context, lam: Rep[A => B], n: IntRep)
////                 (implicit ea: Elem[A], eab: Elem[A => B], lb: Transformer[B]): PA[A=>B] =
////    {
////      val clo = mkClosure(ctx, lam)
////      eab.replicate(n, clo)
////    }
//
//    def vectorizeBinOp[A](ctx: Context, t: Rep[A])(implicit ea: Elem[A]): Rep[A => A => A] = {
////      vectorizedSyms.get(t) match {
////        case Some(res) => res.asInstanceOf[Rep[A => A => A]]
////        case None => {
//          val tV = t match {
//            case Def(op: BinOp[A]) =>
//              val scalar = mkLambda2((x:Rep[A]) => (y: Rep[A]) => op.copyWith(x, y))
//              val lifted = mkLambda2((xs:PA[A]) => (ys:PA[A]) => ExpBinopArray(op, xs, ys))
//              val opS = mkLambda2((e:Rep[Unit]) => (x: Rep[A]) => Clo(x, scalar, lifted))
//              val opL = mkLambda2((e:PA[Unit]) => (xs: PA[A]) => ExpFuncArray(xs, scalar, lifted))
//              val opV = toExp(Clo(toRep(()), opS, opL))
//              opV
//            case _ => ???("BinOp expected", t)
//          }
////          vectorizedSyms ++= Map(t -> tV)
//          tV
////        }
////      }
//    }
//
//    def liftBinOp[A](ctx: Context, t: Rep[A], n: Rep[Int])(implicit ea: Elem[A]): PA[A => A => A] = {
//      val Clo(e, fS, fL) = extractClo[Unit, A, A => A](vectorizeBinOp(ctx, t))
//      //implicit val envElem = e.Elem.getElem
//      ExpFuncArray(unitElement.replicate(n, e), fS, fL)
//    }
//
//    object Var {
//      def unapply[T](e: Exp[T]): Option[Sym[T]] = e match {
//        case s: Sym[T] if findDefinition(s).isEmpty => Some(s)
//        case _ => None
//      }
//    }
//
//    private var liftedSyms = Map.empty[Rep[_], Rep[_]]
//
//    def resolveTransformer[A](e: Elem[A]): Transformer[A] = e match {
//      case p: PairElem[_,_] => pairTransformer(resolveTransformer(p.ea), resolveTransformer(p.eb))
//      case f: FuncElem[_,_] => funcTransformer(resolveTransformer(f.ea), resolveTransformer(f.eb))
//      case s: StdElem[_]    => stdTransformer(s)
//      case u: UnitElement   => stdTransformer(u)
//    }
//
//    def doVectorize[A](ctx: Context, t: Rep[A]): Rep[A] = {
//      ctx.get(t) match {
//        case Some(t1) => t1.asInstanceOf[Rep[A]]
//        case None =>
//          val l = resolveTransformer(t.Elem.getElem)
//          val res = l.vectorize(ctx, t)
//          res equals t match {
//            case false => ctx.add(t, res)
//            case _ =>
//          }
//          res
//      }
//    }
//    def doLift[A](ctx: Context, t: Rep[A], n: Rep[Int]): PA[A] = {
//      ctx.get(t) match {
//        case Some(t1) => t1.asInstanceOf[PA[A]]
//        case None =>
//          val l = resolveTransformer(t.Elem.getElem)
//          val res = l.lift(ctx, t, n)
//          res equals t match {
//            case false => ctx.add(t, res)
//            case _ =>
//          }
//          res
//      }
//    }
//  }
//
//  import Transformer._
//
//  trait Transformer[A] {
//    implicit def elem: Elem[A]
//    implicit val thisTransformer = this
//    def lift(sub: Context, t: Rep[A], n: Rep[Int]): PA[A] = ???("don't know how to lift", t)
//    def vectorize(sub: Context, t: Rep[A]): Rep[A] =  ???("don't know how to vectorize", t)
//  }
//
//  def stdTransformer[A](implicit ea: Elem[A]): Transformer[A] = new Transformer[A] {
//    val elem = ea
//
//    override def vectorize(ctx: Context, t: Rep[A]): Rep[A] = t match {
//      case Def(Const(_)) => t
//      case Def(op: BinOp[A]) => {
//        val xV = doVectorize(ctx, op.lhs)
//        val yV = doVectorize(ctx, op.rhs)
//        op.copyWith(xV, yV)
//        //val opV = vectorizeBinOp(ctx, t)
//        //doApply(doApply(opV, xV), yV)
//      }
//      case Var(_) =>
//        !!!("Cannot vectorize variable: substitution not defined: ", t)
////        (sub contains t) match {
////          case true => (sub \ t).asInstanceOf[Rep[A]]
////          case false => !!!("Cannot vectorize variable: substitution not defined: ", t)
////        }
//      case _ => super.vectorize(ctx, t)
//    }
//
//    override def lift(sub: Context, t: Rep[A], n: Rep[Int]): PA[A] =  t match {
//      case Def(Const(_)) => elem.replicate(n, t)
//
//      case Def(op: BinOp[A]) => {
//        val xL = doLift(sub, op.lhs, n)
//        val yL = doLift(sub, op.rhs, n)
//        ExpBinopArray(op, xL, yL)
//        //val opL = liftBinOp(sub, t, n)
//        //doApplyPA(doApplyPA(opL, xL), yL)
//      }
//
//      case Var(_) =>
//        !!!("Cannot lift variable: substitution not defined: ", t)
////        (sub contains t) match {
////          case true => (sub \ t).asInstanceOf[PA[A]]
////          case false => !!!("Cannot lift variable: substitution not defined: ", t)
////        }
//      case _ => super.lift(sub, t, n)
//    }
//  }
//
//  private var vectorizedLambdas: Map[Rep[_], Rep[_]] = immutable.Map.empty[Rep[_], Rep[_]]
//
//  def funcTransformer[A, B](implicit lA: Transformer[A], lB: Transformer[B]): Transformer[A=>B] = new Transformer[A=>B] {
//    val elem = implicitly[Elem[A=>B]]
//
//    override def vectorize(sub: Context, f: Rep[A=>B]): Rep[A=>B] = f match {
//      case Def(l: Lambda[_,_]) =>
//        vectorizedLambdas.get(f) match {
//          case Some(clo) => clo.asInstanceOf[Rep[A => B]]
//          case None => {
//            val fV: Rep[A=>B] = mkClosure(sub, f)
//            vectorizedLambdas ++= Map(f -> fV)
//            fV
//          }
//        }
//      case Def(c@Clo(_,_,_)) => f
//      case _ => super.vectorize(sub, f)
//    }
//  }
//
//  def pairTransformer[A, B](implicit lA: Transformer[A], lB: Transformer[B]): Transformer[(A,B)] = new Transformer[(A,B)] {
//    val elem = implicitly[Elem[(A,B)]]
//    override def transform(sub: Context, t: Rep[(A,B)], n: Rep[Int]): PA[(A,B)] =  t match {
//      case Def(Tup(a, b)) =>  doTransform(sub, a, n) zip doTransform(sub, b, n)
//      case _ => super.transform(sub, t, n)
//    }
//  }
//
//}
