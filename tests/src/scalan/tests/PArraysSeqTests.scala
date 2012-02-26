package scalan.tests

import org.junit.Assert._
import scalan.dsl._
import scalan.sequential.ScalanSequential
import org.hamcrest._
import core._
import Is._
import scalan.util.{DocumentExtensions => PA}
import scalan.samples._
import org.junit.{Assert, Test}
//import scalan.staged.ScalanStaged

//object StagedArrays extends ScalanStaged
object SeqArrays extends ScalanSequential

//TODO implement equality for PArray
//TODO implement remaining primitives
//TODO get rid of code duplication in different PA implementations (employ scala collections pattern)

class PArraysSeqTests  {
  val samples = new ScalanSequential with DslSamples { override val isDebug = false }
  import samples._

  @Test def pairsNestedReplicate = {
    val arr = replicate(3, ((1,2),3))
    println(arr)
    assertThat(arr.length, is(3))
    val nested = replicate(3, arr)
    println(nested)
    assertThat(nested.length, is(3))
    val s = slice(nested, 1, 2)
    println(s)
    assertThat(s.length, is(2))
  }

  @Test def nestedArrayOperations = {
    val arr = fromArray(Array(1,2,3))
    val nested = replicate(3, arr)
    println(nested)
    assertThat(nested.length, is(3))
    val s = slice(nested, 1, 2)
    println(s)
    assertThat(s.length, is(2))

    val nested2 = replicate(2, nested)
    println(nested2)
    assertThat(nested2.length, is(2))

    val nested3 = replicate(2, nested2)
    println(nested3)
    assertThat(nested3.length, is(2))

    val nested4 = replicate(2, nested3)
    println(nested4)
    assertThat(nested4.length, is(2))
  }

  @Test def concatOperations = {
    val arr = fromArray(Array(1, 2, 3))
    val nested = replicate(3, arr)
    val conc = concat(nested)
    assertThat(conc.length, is(9))
    val backToNested = unconcat(nested)(conc)
    assertThat(backToNested.length, is(3))

    val nested2 = replicate(2, nested)
    val conc2 = concat(concat(nested2))
    assertThat(conc2.length, is(18))
  }

  @Test def sumArray = {
    val v1:(Int|Float) = Left(2)
    val v2: (Int|Float) = Right(5.5f)
    val a = Array(v1,v2)
    val arr = fromArray(a)
    println(arr)
    val arr2 = replicateSeg(3, arr)
    assertThat(arr2.length, is(6))
    val nested = replicate(2,arr2)
    assertThat(nested.length, is(2))

    val flags = arr.map {case Left(i) => false; case Right(f) => true }
    val sumSplitted = flagSplit(flags, arr)
    println(sumSplitted)

  }
  @Test def splits = {
    // A
    val arr = fromArray(Array(1, 2, 3))
    val flags = fromArray(Array(true, false, true))
    val (t,f) = flagSplit(flags, arr)
    val newArr = flagMerge(flags,t,f)

    //(A,B)
    val arrP = fromArray(Array((1,10), (2,20), (3,30)))
    val (tP,fP) = flagSplit(flags, arrP)
    val newArrP = flagMerge(flags,tP,fP)

    // (A|B)
    val v1:(Int|Float) = Left(1)
    val v2: (Int|Float) = Right(2.5f)
    val v3: (Int|Float) = Right(3.5f)
    val aS = Array(v1,v2,v3)
    val arrS = replicateSeg(2, fromArray(aS))
    val flagsS = replicateSeg(2, fromArray(Array(true, false, true)))
    val (tS,fS) = flagSplit(flagsS, arrS)
    val newArrS = flagMerge(flagsS, tS, fS)
    val s = slice(newArrS, 0, 3)

    // PA[A]
    val nested = tabulate(3)(i => {
      val res = arr.map {j => (arr.length*i) + j+1}
      res
    })
    val (tN,fN) = flagSplit(flags, nested)
    val newArrN = flagMerge(flags,tN,fN)
    val tabSeg = tabulateSeg(3)(i => nested(i))
  }

  @Test def comprehensions = {
    val arr = fromArray(Array(1, 2, 3))
    val m = for (e <- arr) yield e
    val arr2 = fromArray(Array(1,2,3))
    val m2 = for (
      e <- arr;
      e2 <- arr2
    ) yield e + e2
  }

  @Test def treeArrays = {

    type IntTree = Tree[Int]

    val t1 = Tree(10, element[IntTree].empty)
    val arr1 = replicate(2, t1)
    val t2 = Tree(20, arr1)
    val arr2 = replicate(2, t2)
    val t3 = Tree(30, arr2)
    val t4 = Tree(40, fromArray(Array(t3,t2)))
    val t5 = Tree(50, replicate(2, t4))
  }

  @Test def dslSamples = {
    import scalan.dsl._
    import scalan.sequential._
    //import scalan.util.{DocumentExtensions => PA}
    import scalan.samples._

    object SSamples extends StdSamples
    val a1 = Array(10f, 20f, 30f)
    val a2 = Array(10f, 10f, 10f)
    val v1 = fromArray(a1)
    val v2 = fromArray(a2)
    //val prod = dotProduct2(v1, v2)
    val stdprod = SSamples.dotProduct(a1,a2)

    val s = vectorOfSquares(5)

    val rowinds = fromArray(Array(0, 2))
    val rowvals = fromArray(Array(5.5f, 6.6f))
    val row = rowinds zip(rowvals)
    val matr = replicate(2,row)
    println(matr)
    val vec = replicate(3, 0.5f)
    val res = matrixVectorMul(matr, vec)

    val unsorted = fromArray(Array(2, 4, 1, 123,  9, 39, 15, 14, 15, 34,23,56,234,2343))
    //val sorted = qsort(unsorted)

  }

//  @Test def barnesHutSample = {
//    import scalan.dsl._
//    object Samples extends BarnesHut with ScalanSequential
//    import Samples._
//
//    val m = 1.0f
//    val vel = (0f, 0f)
//    val ps = fromArray(Array(
//      ((m,(12f,12f)), vel),
//      ((m,(6f,10f)), vel),
//      ((m,(6f,14f)), vel),
//      ((m,(10f,6f)), vel),
//      ((m,(14f,2f)), vel),
//      ((m,(7f,7f)), vel),
//      ((m,(5f,7f)), vel),
//      ((m,(3f,3f)), vel),
//      ((m,(3f,1f)), vel)
//    ))
//    val area = ((0f,0f),(16f,16f))
//    val sub = splitArea(area)
//    val in = inArea(sub(2), ps(0))
//    val tree = buildTree(area, ps)
//    val str = PA.format(tree.toDoc)
//    val treeArr = replicate(2, tree)
//    val strarr = PA.format(treeArr.toDoc)
//
//    val vec = (10f,10f)
//    val moved = moveTree(tree, vec)
//    val movedStr = PA.format(moved.toDoc)
//  }
//
//  @Test def astSamples = {
//    import scalan.dsl._
//    object Samples extends AstSamples with ScalanSequential
//    import Samples._
//
//    val x = Var("x")
//    val y = Var("y")
//    val c1 = Con("C1", x, y)
//    val c2 = Con("C2", c1, 1, 2, 3)
//    val c3 = Con("C3", c2, c1, 10, 20)
//    val sub = applySubstitution(c3, Map("x" -> 1000))
//    val sub2 = applySubstitution(sub, Map("y" -> 2000))
//
//    val f = Lam("x", App(Var("plus"), Con("P", x, 1)))
//    val app = App(f, 100)
//    val app2 = App(f, f)
//    eval(app, emptyArrayOf[Expr])
//
//    val apps = replicate(10, app)
//    val res = apps map { eval(_, emptyArrayOf[Expr]) }
//  }
}


import scalan.dsl._
import SeqArrays._


