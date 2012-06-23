package scalan.samples

import scalan.dsl._

trait DslSamples extends Scalan {

  def odd(x: Rep[Int]) = !(x % toRep(2) == toRep(0))

  def vectorOfSquares(len: Int):PA[Int] = for (x <- fromArray(toRep((0 to len).toArray))) yield (x * x)
  def vectorOfSquaresOdd(len: Int):PA[Int] = for (x <- fromArray(toRep((0 to len).toArray)) if odd(x)) yield (x * x)

  type VectorElem = (Int,Float)
  type SparseVector = PArray[VectorElem]
  type Vector = PArray[Float]
  type Matrix = PArray[SparseVector]

  def dotProduct(v1: Rep[Vector], v2: Rep[Vector]): Rep[Float] =
    sum((v1 zip v2) map { case Pair(f1, f2) => f1 * f2 })

//  def dotProduct(v1: Rep[PArray[Rep[Float]]], v2: Rep[PArray[Rep[Float]]]): Rep[Float] =
//    sum((v1 zip v2) map { case Pair(f1, f2) => f1 * f2 })

  //def dotProduct2(v1: Vector, v2: Vector): Rep[Float] = (fromRep(v1), fromRep(v2)).zippedPA.map{_ * _}.sum

  def sparseVectorMul(sv: Rep[SparseVector], v: Rep[Vector]) =
    sum(sv map { case Pair(i,value) =>  v(i) * value })
    //sum(sv map { p => { val (i,value) = lift(p);  v(i) * value} })

  //def matrixVectorMul(mat: Rep[Matrix], vec: Rep[Vector]) = mat map {row => sparseVectorMul(row, vec)}
  def matrixVectorMul(mat: Rep[Matrix], vec: Rep[Vector]) =
     for (row <- mat) yield sparseVectorMul(row, vec)

  lazy val qsort = letrec((qs: Rep[PArray[Int] => PArray[Int]]) => (xs: PA[Int]) => {
    val len = xs.length
    if (len <= 1) xs
    else {
      val m = xs(len / 2)
      val smaller = for (x <- xs if x < m) yield x     // can use for-comprehesions
      val greater = xs filter(x => x > m)              // or methods directly
      val equal = xs filter(_ == m)                    // or even shorter
      val subs = nestArrays(smaller, greater)
      val sorted = for (sub <- subs) yield qs(sub)
      sorted(0) ++ equal ++ sorted(1)
    }
  })

  //implementation using partition
  lazy val qsort1 = letrec((qs: Rep[PArray[Int] => PArray[Int]]) => (xs: PA[Int]) => {
    val len = xs.length
    if (len <= 1) xs
    else {
      val pivot = xs(len / 2)    
      val ps = replicate(len, pivot)
      val less = xs.zip(ps) map { case Pair(x, p) => x < p }
      val subs = xs.partition(less)
      val sorted = subs map { sub => qs(sub) }
      sorted.values
    }
  })

  lazy val qsortV = letrec((qs: Rep[PArray[Int] => PArray[Int]]) => (xs: PA[Int]) => {
    val len = xs.length
    if (len <= 1) xs
    else {
      val pivot = xs(len / 2)
      val ps = replicate(len, pivot)
      val less = xs.zip(ps) map { case Pair(x, p) => x < p }
      val subs = xs.partition(less)
      val sorted = subs map { sub => qs(sub) }
      sorted.values
    }
  })

}

trait StdSamples {
  type VectorElem = (Int,Float)
  type SparseVector = Array[VectorElem]
  type Vector = Array[Float]
  type Matrix = Array[SparseVector]

  def dotProduct(v1: Vector, v2: Vector): Float = (for ((f1,f2) <- v1 zip v2) yield f1 * f2).sum

  def dotProduct2(v1: Vector, v2: Vector): Float = (v1, v2).zipped.map{ _ * _ }.sum
}

