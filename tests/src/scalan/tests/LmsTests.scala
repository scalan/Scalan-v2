package scalan.tests

import org.junit.{Assert, Test}
import org.junit.Assert._
import org.hamcrest._
import core._
import Is._
import scalan.staged.{ScalanExportGraph, ScalanStaged, StagedImplementation}

/**
 * Tests that specify behavior of virtualized Scala and LMS core library we rely on
 */
class LmsTests extends FileDiffSuite with ScalanSamples { //extends ScalaOpsPkgExp {
  val scln = new ScalanStaged with ScalanExportGraph { override val isDebug = false }
  val prefix = "test-out/LmsTests/"
  import scln._

  @Test def Equality_of_Exp = {
    val x: Exp[Int] = toRep(10)
    val y: Exp[Int] = toRep(10)

    assertThat(x eq y, is(true))  // equal constants represented by the same Sym
    assertThat(x equals y, is(true))
    //assertThat(x == y, is(toRep(true)))

    val z: Exp[Int] = toRep(20)

    assertThat(x eq z, is(false))
    assertThat(x equals z, is(false))
    //assertThat(x == z, is(toRep(false)))

    val zz = z

    assertThat(z eq zz, is(true))
    assertThat(z equals zz, is(true))
    //assertThat(z == zz, is(toRep(true)))

    val a: Exp[Array[Int]] = toRep(Array(1, 2, 3))
    val aa = a
    assertThat(a eq aa, is(true))
    assertThat(a equals aa, is(true))
    //assertThat(a == aa, is(toRep(true)))

    val b: Exp[Array[Int]] = toRep(Array(1, 2, 3))
    assertThat(a eq b, is(true))       // equal arrays are represented by the same Sym
    assertThat(a equals b, is(true))
    //assertThat(a == b, is(toRep(false)))
  }

  @Test def implicitConversionToExp = {
    //implicit def valueToExp[T](x: T)(implicit p: Pure[T, Rep]): Rep[T] = toRep(x)

    val x = toRep(10)
    val y: Rep[Int] = 10

    assertThat(x eq y, is(true))
    assertThat(x equals y, is(true))
    //assertThat(x == y, is(toRep(true)))
  }

  case class TestDef(a: Exp[Int], b: Exp[Array[Int]]) extends Def[Int]

  @Test def Equality_of_Symbols_and_Defs = {
    //implicit def valueToExp[T](x: T)(implicit p: Pure[T, Rep]): Rep[T] = toRep(x)
    val a: Rep[Array[Int]] = Array(1,2,3)
    val d1 = TestDef(10, a)
    val d2 = TestDef(10, a)

    assertThat(d1 eq d2, is(false))
    assertThat(d1 == d2, is(true))    // due to structural equality

    val x: Exp[Int] = d1
    val y: Exp[Int] = d2

    assertThat("equal definitions give eq symbols", x eq y, is(true))   // reference equality!!!  thanks to Sym search in the globalDefs (via toExp)
    //assertThat(x == y, is(toRep(true)))

    // Arrays are not compared structurally (but we redefined equality for Const[Array[T]])
    val d3 = TestDef(10, Array(1,2,3))

    assertThat(d2 eq d3, is(false))
    assertThat(d2 equals d3, is(true))

    val z: Exp[Int] = d3
    assertThat(x eq z, is(true))
    //assertThat(x == z, is(toRep(false)))

    // defs with symbols
    val d4 = TestDef(x, a)
    val d5 = TestDef(y, a)
    assertThat(d4 equals d5, is(true))

    // defs with one array that is automatically lifted to const twice
    val arr = Array(1,2,3)
    val d6 = TestDef(1, arr)
    val d7 = TestDef(1, arr)
    assertThat(d6 == d7, is(true))

    // composition of defs
    val d8 = TestDef(d4, arr)
    val d9 = TestDef(d5, arr)
    assertThat(d8 == d9, is(true))
  }

  @Test def Equality_of_Lambda = {
    val x = fresh[Int]
    val b = incFunc(x)
    val l1 = Lambda(incFunc, x, b)
    val l2 = Lambda(incFunc, x, b)
    assertThat(l1 equals l2, is(true))

    val l3 = Lambda(inc2Func, x, b)     // function doesn't matter
    assertThat(l1 equals l3, is(true))

    val y = fresh[Int]
    val b2 = incFunc(y)
    val l4 = Lambda(incFunc, y, b2)
    assertThat(l1 equals l4, is(false))

    val plus = mkLambda2(plusFunc)
    testUnification(plus, false)

    val plus2 = mkLambda3(plus2Func)
    testUnification(plus2, false)

//    withOutFile(prefix+"plus.txt") {
//      emitDepGraph(plus, prefix + "plus.dot", false)
//    }
  }

  def testUnification(lam: Rep[_], equal: Boolean = true) = {
    val Def(inc@Lambda(f, x, body)) = lam
    var b2 = f(x)
    assertThat(body equals b2,  is(equal))
  }

  @Test def Defs_Unification = {
    val inc = mkLambda(incFunc)
    val Def(Lambda(f, x, body)) = inc
    assertEquals(f, incFunc)

    testUnification(inc)

    val inc2 = mkLambda(inc2Func)
    testUnification(inc2)
    val square = mkLambda(squareFunc)
    testUnification(square)

    val plus = mkLambda2(plusFunc)
    withOutFile(prefix+"plus.txt") {
      emitDepGraph(plus, prefix + "plus.dot", false)
    }

    testUnification(plus, false)

    testUnification(mkLambda3(plus2Func), false)
  }
}
