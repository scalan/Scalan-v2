package scalan.samples

//import HLanguage._
import scalan.dsl._

//trait AstTypes extends Scalan {
//  type Expr = Tree[ExprNode]
//  type ExprNode =
//    ( String |                     // Variables x, y, ... or env x -> expr
//    ( Int    |                     // Int constant 10, 20, ...
//    ( String |                     // constructor C(args)
//    ( String |                     // lambda abstraction \x -> Expr
//    ( Unit   |                     // application (e1 e2)
//      String  )))))                // closure item  x -> expr
//
//  type Env = PA[Expr]
//
//  def exprElem: Elem[Expr] = element[Expr]
//}
//
//trait AstSamples extends AstTypes {
//
//  implicit def exprE: Elem[Expr] = exprElem
//  //implicit def valueE: Elem[Value] = valueElem
//
////  type Env = String => Value
////  case class Value(e: Expr, env: Env)
//
//  implicit def pimpEnv(env: Env): EnvW = new EnvW(env)
//  class EnvW(val env: Env)  {
//    def apply(varName: String): Expr = {
//      val (vs, _) = unzipTree(env)
//      val res =  env filter { case EnvItem(name, e) => name == varName }
//      if (res.length == 0) error("variable " + varName + " not found in environment " + vs)
//      else {
//        val EnvItem(_, e) = res(0)
//        e
//      }
//    }
//  }
//  object PA {
//      def unapplySeq(es: PA[Expr]): Option[Array[Expr]]  = Some(es.toArray)
//  }
//
//  private val closureVarName = "$closure$"
//  object Var {
//    def apply(name: String): Expr = {
//      if (name == closureVarName) error("invalid var name: " + name);
//      Tree(Left(name))
//    }
//    def unapply(e: Expr) = e.value fold(v => if (v != closureVarName) Some(v) else None, _ => None)
//  }
//  object Clo {
//    def apply(e: Expr, env: Env): Expr = Tree(Left(closureVarName), singleton(e) ++ env)
//    def unapply(e: Expr) = e.value fold(
//      v =>
//        if (v == closureVarName) {
//          val expr = e.children(0)
//          val env = e.children.slice(1, e.children.length-1)
//          Some((expr, env))
//        }
//        else None,
//      _ => None)
//  }
//  object Lit {
//    def apply(n: Int): Expr = Tree(Right(Left(n)))
//    def unapply(e: Expr) = e.value fold(_ => None, _.fold(n => Some(n), _ => None))
//  }
//  implicit def toLit(n: Int): Expr = Lit(n)
//
//  object Con {
//    def apply(name: String, args: PA[Expr]): Expr = Tree(Right(Right(Left(name))), args)
//    def apply(name: String, args: Expr*): Expr = apply(name, fromArray(Array(args: _*)))
//    def unapply(e: Expr) = e.value fold(_ => None,
//      _.fold(_ => None,
//      _.fold(c => Some((c, e.children)), _ => None)))
//  }
//  object Lam {
//    def apply(v: String, body: Expr): Expr = Tree(Right(Right(Right(Left(v)))), singleton(body))
//    def unapply(e: Expr) = e.value fold(_ => None,
//      _.fold(_ => None,
//      _.fold(_ => None,
//      _.fold(l => Some((l, e.children(0))), _ => None))))
//  }
//  object App {
//    def apply(a: Expr, h: Expr): Expr = Tree(Right(Right(Right(Right(Left())))), fromArray(Array(a, h)))
//    def unapply(e: Expr) = e.value fold(_ => None,
//      _.fold(_ => None,
//      _.fold(_ => None,
//      _.fold(_ => None,
//      _.fold(_ => Some((e.children(0),e.children(1))), _ => None)))))
//  }
//  object EnvItem {
//    def apply(name: String, v: Expr): Expr = Tree(Right(Right(Right(Right(Right(name))))), singleton(v))
//    def unapply(e: Expr) = e.value fold(_ => None,
//      _.fold(_ => None,
//      _.fold(_ => None,
//      _.fold(_ => None,
//      _.fold(_ => None, name => Some((name,e.children(0))))))))
//  }
//
//  def applySubstitution(term: Expr, s: Map[String, Expr]): Expr = term match {
//    case l@Lit(_) => l
//    case v@Var(name) => s.get(name) match { case Some(t) => t; case None => v }
//    case c@Con(name, args) => Con(name, args map {applySubstitution(_, s)})
//    case l@Lam(v, t) =>
//      Lam(v, applySubstitution(t, s - v))
//    case App(h, a) => {
//      val subs = for (e <- fromArray(Array(h, a))) yield applySubstitution(e, s)
//      App(subs(0), subs(1))
//    }
////    case CaseExpression(sel, bs) =>
////      CaseExpression(applySubstitution(sel, s),
////          bs map {b => Branch(b.pattern, applySubstitution(b.term, s -- b.pattern.args))})
////    case let: LetExpression => throw new IllegalArgumentException("unexpected expr: " + let)
////    case letrec: LetRecExpression => throw new IllegalArgumentException("unexpected expr: " + letrec)
//  }
//  val primOps = Set("plus", "minus")
//  def isPrim(opName: String) = primOps.contains(opName)
//
//  def eval(expr: Expr, env: Env): Expr = expr match {
//    case l@Lit(n) => l
//    case v@Var(x) => if (isPrim(x)) v else env(x)
//    case Con(name, args) => Con(name, args map { eval(_, env) })
//    case App(f, g) => {
//      val vs = fromArray(Array(f, g)) map { eval(_, env) }
//      val (f1,g1) = (vs(0), vs(1))
//      f1 match {
//        case Clo(Lam(x, e1), env1) =>
//          val env2 = singleton(EnvItem(x, g1)) ++ env1
//          eval(e1, env2)
//        case Var("plus") => g1 match {
//          case Con(name, args) if name == "P" && args.length == 2 => (args(0), args(1)) match {
//            case (Lit(a1),Lit(a2)) => Lit(a1 + a2)
//          }
//          case _ => error("constructor P expected: operation 'plus'")
//        }
//        case _ => error("expression doesn't evaluate to lambda abstraction or  primitive function name " + f)
//      }
//    }
//    case Lam(_,_) => Clo(expr, env)
//  }
//
//  implicit def toExpression(e: Expr): Expression = e match {
//    case Var(name) => Variable(name)
//    case Lit(n) => IntLiteral(n)
//    case Con(name, args) => Constructor(name, args.toArray.toList map {toExpression(_)})
//    case Lam(v, t) => LambdaAbstraction(Variable(v), toExpression(t))
//    case App(h, a) => Application(toExpression(h), toExpression(a))
//    case _ => error("unknown expr: " + e.value)
//  }
//}

