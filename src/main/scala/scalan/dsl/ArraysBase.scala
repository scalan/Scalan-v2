package scalan.dsl
import scala.text._
import Document._
import scalan.util.DocumentExtensions._
import scalan.common._
import Common._
import virtualization.lms.common.{Functions, IfThenElse, Equal, Pure}
import annotation.implicitNotFound
import virtualization.lms.util.OverloadHack

trait ArraysBase extends ScalanBase
                    with ScalanStdArrayOps
                    with PArrayOps
                    with ScalanTuples
                    with ScalanEqual
                    with ScalanIfThenElse
                    with Functions
                    with OverloadHack
{
  type PA[A] = Rep[PArray[A]]   // parallel array of A
  type Elem[A] = Element[A]    // type class of array element
  type |[A,B] = Either[A,B]
  type IntRep = Rep[Int]
  type BoolRep = Rep[Boolean]
  type UnitRep = Rep[Unit]
  type ByteRep = Rep[Byte]
  type ShortRep = Rep[Short]
  type CharRep = Rep[Char]
  type LongRep = Rep[Long]
  type FloatRep = Rep[Float]
  type DoubleRep = Rep[Double]
  type NArray[A] = PArray[PArray[A]]

  @implicitNotFound(msg = "No Element available for ${A}.")
  trait Element[A] {
    def replicate(count: IntRep, v: Rep[A]): PA[A]               // (3,v) -> (v,v,v)
    def replicateSeg(count: IntRep, v: PA[A]): PA[A]             // (3,(a,b)) -> (a,b,a,b,a,b)
    def tabulate(len: IntRep)(f:IntRep => Rep[A]): PA[A]
    def tabulateSeg(len: IntRep)(f:IntRep => PA[A]): PA[A]
    def empty: PA[A]
    def manifest: Manifest[A]
    def zero: Zero[A]
    def name = manifest.toString

    def defaultOf = mzero[A](zero)
    def singleton(v:Rep[A]) = replicate(element[Int].toRep(1),v)
    def fromArray(arr: Rep[Array[A]]) = {
      implicit val elemA: Elem[A] = this
      tabulate(arr.length)(i => arr(i))
    }
    def toRep(x: A): Rep[A]
    
    //Element.bindWithManifest(manifest, this)
  }
//  object Element {
//    def fromManifest[A](m: Manifest[A]): Elem[A] = {
//      _m2elem.getOrElse(m, { ???("Cannot find element for Manifest " + m) }).asInstanceOf[Elem[A]]
//    }
//
//    private val _m2elem = scala.collection.mutable.Map[Manifest[_], Elem[_]]()
//
//    private def bindWithManifest[A](m: Manifest[A], e: Elem[A]) {
//      _m2elem.contains(m) match { case true => _m2elem += (m -> e) case _ => }
//    }
//  }

  trait PArray[T] {
    def length: IntRep
    def index(i: IntRep): Rep[T]
    def toArray: Rep[Array[T]]
    def map[B:Elem](f: Rep[T] => Rep[B]): PA[B]
    def map2[B:Elem](f: Rep[T => B]): PA[B] = ???
    def flatMap[B:Elem](f:Rep[T] => PA[B]): PA[B]
    def filter(f: Rep[T] => BoolRep): PA[T] = { val flags = for (e <- this) yield f(e); pack(flags) }
    def withFilter(f: Rep[T] => BoolRep): PA[T]
    def apply(i: IntRep): Rep[T] = index(i)

    def zip[B](b: PA[B])(implicit eb:Elem[B], etb: Elem[(T,B)]): PA[(T, B)]

    def zipWith[B,C]
          (f: Rep[T] => Rep[B] => Rep[C])(that: PA[B])
          (implicit eb:Elem[B], ec:Elem[C], etb: Elem[(T,B)]): PA[C]

    def backPermute(idxs: PA[Int]): PA[T]
    def permute(idxs: PA[Int]): PA[T]

    def slice(start:IntRep, len:IntRep): PA[T]
    def scan(implicit s:Monoid[T]): PA[T]                     // (1,2,3,4,5) -> (0,1,3,6,10)
    def pack(flags: PA[Boolean]): PA[T]
    def expandBy[B:Elem](nested: PA[PArray[B]]): PA[T]
    def partition(flags: PA[Boolean]): PA[PArray[T]]
    def sum(implicit m: Monoid[T]): Rep[T]
    /**
     * length(this) == length(ifFalse) == length(flags)
     */
    def flagCombine(ifFalse: PA[T], fs: PA[Boolean]): PA[T]

    def flagMerge  (ifFalse: PA[T], flags: PA[Boolean]): PA[T]  // length(this) + length(ifFalse) == length(flags)
    def flagSplit  (flags: PA[Boolean]): Rep[(PArray[T], PArray[T])]         // length(this) == length(flags) == (length(A) + length(B))

    def ++(that: PA[T])(implicit epa:Elem[PArray[T]]): PA[T]
    def toDoc: Document = text(toString)

    implicit val elem: Elem[T]
  }

  def emptyArrayOf[T:Elem] = element[T].empty

  def element[A](implicit ea:Elem[A]): Elem[A] = ea

  implicit val boolElement:  Elem[Boolean]
  implicit val intElement:   Elem[Int]
  implicit val floatElement: Elem[Float]
  implicit val unitElement:  Elem[Unit]
  implicit val stringElement:  Elem[String]
  implicit def arrayElement[A: Manifest]: Elem[Array[A]]
//  implicit val byteElement:  Elem[Byte]
//  implicit val shortElement: Elem[Short]
//  implicit val longElement:  Elem[Long]
//  implicit val charElement:  Elem[Char]
//  implicit val doubleElement:Elem[Double]

  implicit def pairElement[A,B](implicit ea: Elem[A], eb: Elem[B]): Elem[(A,B)]
  implicit def sumElement [A,B](implicit ea: Elem[A], eb: Elem[B]): Elem[(A|B)]
  implicit def parrayElement[A](implicit a: Elem[A]): Elem[PArray[A]]
  implicit def funcElement[A,B](implicit ea: Elem[A], eb: Elem[B]): Elem[A => B]

  implicit def elem2Zero[A](implicit ea: Elem[A]): Zero[A] = ea.zero

  implicit def fromPA2PArray[A](x: Rep[PArray[A]]): PArray[A]

  implicit def elementType2Pure[A:Elem]: Pure[A,Rep] = new Pure[A,Rep]{ def pure(x: => A) = element[A].toRep(x) }

  def __ifThenElse[T](cond: Rep[Boolean], thenp: => PA[T], elsep: => PA[T])(implicit o: Overloaded1, et: Elem[T]): PA[T]

  /*
  * Abstracted implementation that is defined once and for all Element types
  * */
  trait PArrayBase[T] extends PArray[T] {
    def zipWith[B,C]
          (f: Rep[T] => Rep[B] => Rep[C])(that: PA[B])
          (implicit eb:Elem[B], ec:Elem[C], etb: Elem[(T,B)]): PA[C] =
      this.zip(that) map { case Pair(a, b) => f(a)(b) }

    def backPermute(idxs: PA[Int]): PA[T] = {   //(implicit t: Elem[T], m:Manifest[T])
      val len = idxs.length
      elem.tabulate(len)(i => this(idxs(i)))
    }


    def permute(idxs: PA[Int]): PA[T] = ???
//    {       //(implicit t: Elem[T], m:Manifest[T])
//      implicit val mt = elem.manifest
//      val newArr = new Array(length)
//      for (i <- 0 to (idxs.length-1)) newArr(idxs(i)) = this(i)
//      elem.fromArray(newArr)
//    }

    def flagCombine(ifFalse: PA[T], fs: PA[Boolean]): PA[T] =
        this.zip(ifFalse).zip(fs).map { case Pair(Pair(t,f),flag) => if (flag) t else f }

    def flatMap[B:Elem](f:Rep[T]=>PA[B]): PA[B] = element[B].tabulateSeg(this.length)(i => f(this(i)))

    def withFilter(f: Rep[T]=>Rep[Boolean]): PA[T] = filter(f)

    def scan(implicit s:Monoid[T]): PA[T] = sys.error("scan is not supported for array with elements of type " + this.elem.manifest.toString)

    def pack(flags: PA[Boolean]): PA[T] = { val Pair(res,_) = this.flagSplit(flags); res }
    def sum(implicit m: Monoid[T]): Rep[T]
  }

  trait UnitArray extends PArray[Unit] {
    def length: IntRep
    def index(i: IntRep) = elem.toRep(())
    override def toDoc = group("UnitArray(" + length +  ")" :: ED)
  }

  trait StdArray[A] extends PArray[A] {
     def arr: Rep[Array[A]]
  }

  trait PairArray[A, B] extends PArray[(A,B)] {
    def a: PA[A]
    def b: PA[B]
    def length = a.length
    def index(i: IntRep) = (a(i),b(i))
    override def toDoc = group("PairArray(" :: nest(2,a.toDoc) :: ", " :/: nest(2,b.toDoc) :/: ")" :: ED)
  }

  trait SumArray[A, B] extends PArray[(A|B)] {
    def flags: PA[Boolean]
    def a: PA[A]
    def b: PA[B]
    def length = flags.length
    override def toDoc = group("SumArray(" :: nest(2,flags.toDoc) :: ", " :/: nest(2,a.toDoc) :: ", " :/: nest(2,b.toDoc) :: ")" :: ED)
  }

  trait NestedArray[A] extends PArray[PArray[A]] {
    def arr: PA[A]
    def segments: PA[(Int,Int)]
    def length = segments.length
    def index(i: IntRep) = { val Pair(p,l) = segments(i); arr.slice(p, l) }
    override def toDoc = group("NestedArray(" :: nest(2,arr.toDoc) :: ", " :/: nest(2,segments.toDoc) :: ")" :: ED)
  }

  trait FuncArray[E, A, B] extends PArray[A => B] {
    def env: PA[E]
    def fun: Rep[E => A => B]
    def funl: Rep[PArray[E] => PArray[A] => PArray[B]]
    def length = env.length
    override def toDoc = group("FuncArray("
      :: nest(2,env.toDoc) :: ", " :: nest(2, text(fun.toString)) :: ", " :/: nest(2, text(funl.toString))
      :: ")" :: ED)
  }

  abstract class BaseElem[A] extends Element[A]
  abstract class PairElem[A,B](val ea: Elem[A], val eb: Elem[B]) extends Element[(A,B)]
  abstract class FuncElem[A,B](val ea: Elem[A], val eb: Elem[B]) extends Element[A => B]
  abstract class PArrayElem[A](val ea: Elem[A]) extends Element[PArray[A]]

  def mkStdArray[A](len:IntRep)(f: IntRep => Rep[A])
                   (implicit s: Semigroup[A], ea:Elem[A]): PA[A]

  def mkStdArray[A](len:IntRep, v: Rep[A])(implicit s: Semigroup[A], ea:Elem[A]): PA[A]

  def mkUnitArray(len: IntRep): PA[Unit]
  def mkPairArray[A, B](a: PA[A], b: PA[B])(implicit ea: Elem[A], eb: Elem[B], e:Elem[(A,B)]): PA[(A,B)]
  def mkNestedArray[A](arr: PA[A], segments: PA[(Int,Int)])(implicit ea: Elem[A]): PA[PArray[A]]

  def unzip[A, B](p: PA[(A, B)])(implicit ea: Elem[A], eb: Elem[B]): (PA[A], PA[B])
  def concat[A:Elem](a: PA[PArray[A]]): PA[A]
  def unconcat[A, B](shapeArr: PA[PArray[A]])(arr: PA[B])(implicit ea: Elem[A], eb: Elem[B]): PA[PArray[B]]
  def nestArrays[A:Elem](a1: PA[A], a2: PA[A]): PA[PArray[A]]
}
