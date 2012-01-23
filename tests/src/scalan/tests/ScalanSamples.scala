package scalan.tests

import scalan.staged.{ScalanExportGraph, ScalanStaged}

trait ScalanSamples {
  val scln: ScalanStaged
  import scln._

  val idFunc = (x: Rep[Int]) => x
  val incFunc = (x: Rep[Int]) => x + 1
  val inc2Func = (x: Rep[Int]) => x + 1 + 2
  val squareFunc = (x: Rep[Int]) => x * x
  val plusFunc = (x: Rep[Int]) => (y: Rep[Int]) => x + y
  val plus2Func = (x: Rep[Int]) => (y: Rep[Int]) => (z: Rep[Int]) => x + y + z
  val let0Func = (x: Rep[Int]) => { val y = x + 1; val z = x + y; z * y }
  val letFunc = (x: Rep[Int]) => { val y = x + 1; y * y }
  val let2Func = (x: Rep[Int]) => {
    val y = x + 1;
    val z = x + 2;
    y * z
  }
  val let3Func = (x: Rep[Int]) => {
    val y = x + 1;
    val z = x + 2;
    val q = y + z
    q * y
  }

  def if1Func(x: Rep[Int]): Rep[Int] =
    if (x == 0) toRep(0) else toRep(1)

  def compareFunc(x: Rep[Int])(y: Rep[Int]): Rep[Int] =
    if (x == y) 1 else 0

}

trait ScalanSampleSymbols extends ScalanSamples {
  import scln._

  lazy val id = mkLambda(idFunc)
  lazy val inc = mkLambda(incFunc)
  lazy val inc2 = mkLambda(inc2Func)
  lazy val square = mkLambda(squareFunc)
  lazy val plus = mkLambda2(plusFunc)
  lazy val plus2 = mkLambda3(plus2Func)
  lazy val let0 = mkLambda(let0Func)
  lazy val let = mkLambda(letFunc)
  lazy val let2 = mkLambda(let2Func)
  lazy val let3 = mkLambda(let3Func)
  lazy val if1 = mkLambda(if1Func)
  lazy val compare = mkLambda2(compareFunc)

  lazy val factorial = letrec[Int,Int](f => n => if (n == 1) 1 else n * f(n - 1))

  lazy val even:Rep[Int=>Boolean] = letrec[Int,Boolean](f => n => if (n == 0) true else odd(n - 1))
  lazy val odd:Rep[Int=>Boolean] = letrec[Int,Boolean](f => n => if (n == 0) false else even(n - 1))
}
