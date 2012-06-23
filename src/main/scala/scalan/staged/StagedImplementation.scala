package scalan.staged

import scalan.common._
import Common._
import scalan.dsl._
import virtualization.lms.internal.Expressions
import virtualization.lms.ppl.ScalaOpsPkgExp
import virtualization.lms.common.{ScalaGenEffect, FunctionsExp}
import java.io.PrintWriter
import reflect.SourceContext

trait StagedImplementation extends StagedImplBase
                              with Expressions
                              with StagedFunctions
                              with StagedArithmetic
                              with StagedLogical
                              with StagedSumExp
                              with StagedStdArrayOps
                              with StagedPArrayOps
                              with ScalaOpsPkgExp
{ outer =>
  override implicit lazy val boolElement:  Elem[Boolean] =
    new StagedBaseElement[Boolean]()(
      implicitly[Semigroup[Boolean]], Zero.BooleanZero, manifest[Boolean])

  override implicit lazy val intElement:   Elem[Int] =
    new StagedBaseElement[Int]()(
      implicitly[Semigroup[Int]], Zero.IntZero, manifest[Int])

  override implicit lazy val floatElement: Elem[Float] =
    new StagedBaseElement[Float]()(
      implicitly[Semigroup[Float]], Zero.FloatZero, manifest[Float])

  override implicit lazy val stringElement:Elem[String] =
    new StagedBaseElement[String]()(
      implicitly[Semigroup[String]], Zero.StringZero, manifest[String])

  implicit def arrayElement[A](implicit m: Manifest[A]): Elem[Array[A]] =
    new StagedBaseElement[Array[A]]()(
      implicitly[Semigroup[Array[A]]], Zero.ArrayZero(m), manifest[Array[A]]) {
      override def toRep(x: Array[A]) = Const(x)//ArrayConst(Const(x))
    }

  override implicit lazy val unitElement:  Elem[Unit] = new UnitElement

  class StagedBaseElement[A](implicit s: Semigroup[A], override val zero:Zero[A], m:Manifest[A])
    extends BaseElem[A] with StagedElement[A]
  {
    implicit val elemA = this
    def manifest: Manifest[A] = m

    override def fromArray(arr: Rep[Array[A]]) = ExpStdArray(arr)
    def replicate(count: IntRep, v: Rep[A]): PA[A] = (count,v) match {
      case (Def(Const(_)), Def(Const(_))) => ExpStdArray(ArrayFill(count, v))
      case _ => ReplicatePA(count, v)
    }
    def replicateSeg(count: IntRep, v: PA[A]) = v match {
      case Def(ExpStdArray(a)) => ExpStdArray(ArrayFillSeg(count, a))
      case _ => ReplicateSegPA(count, v)
    }
    def tabulate(len: IntRep)(f:IntRep => Rep[A]) = mkStdArray(len)(f)
    def tabulateSeg(len: IntRep)(f:IntRep => PA[A]) =
      ExpStdArray(ArrayConcat(ArrayTabulate(len, fun {(i:IntRep) => f(i).toArray})))
    def empty = replicate(outer.toRep(0), toRep(mzero[A]))
    def toRep(x: A) = Const(x)
  }

  class UnitElement extends StagedElement[Unit]
  {
    private val m = scala.Predef.manifest[Unit]
    private val z = Zero.UnitZero
    implicit def manifest = m
    def zero = z
    def replicate(count: IntRep, v: Rep[Unit]) = mkUnitArray(count)
    def replicateSeg(count: IntRep, v: PA[Unit]) = mkUnitArray(count * v.length)
    def tabulate(len: IntRep)(f:IntRep => Rep[Unit]) = mkUnitArray(len)
    def tabulateSeg(len: IntRep)(f:IntRep => PA[Unit]) = {
      val lens = mkStdArray(len)(i => f(i).length);
      mkUnitArray(lens.sum)
    }
    def empty = replicate(outer.toRep(0), toRep(()))
    def toRep(x: Unit) = Const(x)
  }


  override implicit def pairElement[A,B](implicit elema: Elem[A], elemb: Elem[B]): Elem[(A,B)] =
    new PairElem[A,B](elema, elemb) with StagedElement[(A,B)]
  {
    implicit val elemAB = this
    private val m = Manifest.classType(classOf[(A,B)], ea.manifest, eb.manifest)
    private val z = Common.zero((ea.defaultOf, eb.defaultOf))
    def manifest: Manifest[(A,B)] = m
    def zero = z

    def replicate(count: IntRep, v: Rep[(A,B)]) = ExpPairArray(ea.replicate(count, v._1), eb.replicate(count, v._2))
    def replicateSeg(count: IntRep, v: PA[(A,B)]) = {
      val (a,b) = unzip(v)
      ExpPairArray(ea.replicateSeg(count, a), eb.replicateSeg(count, b))
    }
    def tabulate(len: IntRep)(f:IntRep => Rep[(A,B)]) = {
      val fA = (i: IntRep) => unzipPair(f(i))._1
      val fB = (i: IntRep) => unzipPair(f(i))._2
      val a1 = ea.tabulate(len)(fA)
      val a2 = eb.tabulate(len)(fB)
      ExpPairArray(a1, a2)
    }
    def tabulateSeg(len: IntRep)(f:IntRep => PA[(A,B)]) = {
      val fA = (i: IntRep) => unzip(f(i))._1
      val fB = (i: IntRep) => unzip(f(i))._2
      val a1 = ea.tabulateSeg(len)(fA)
      val a2 = eb.tabulateSeg(len)(fB)
      ExpPairArray(a1, a2)
    }
    def empty = ExpPairArray(ea.empty, eb.empty)
    def toRep(p: (A,B)) = Pair(ea.toRep(p._1), eb.toRep(p._2))
  }

  override implicit def sumElement [A,B](implicit elema: Elem[A], elemb: Elem[B]): Elem[(A|B)] =
    new SumElem[A,B](elema, elemb) with StagedElement[(A|B)]
    {
      implicit val elemAB = this
      lazy val boolElem = element[Boolean]
      lazy val m: Manifest[(A|B)] = Manifest.classType(classOf[(A|B)], ea.manifest, eb.manifest)
      def zero = Common.zero(scala.Left(ea.defaultOf))
      def manifest: Manifest[(A|B)] = m

      def replicate(count: IntRep, v: Rep[(A|B)]) = {
        v.fold(
          a => {
            val as = ea.replicate(count, a)
            ExpSumArray(boolElem.replicate(count, false), as, eb.empty)
          },
          b => {
            val bs = eb.replicate(count, b)
            ExpSumArray(boolElem.replicate(count, true), ea.empty, bs)
          }
        )
      }

      def replicateSeg(count: IntRep, v: PA[(A|B)]) = {
        ExpSumArray(
          boolElem.replicateSeg(count, v.flags),
          ea.replicateSeg(count, v.a),
          eb.replicateSeg(count, v.b))
      }

      def tabulate(len: IntRep)(f:IntRep => Rep[(A|B)]) = {
        val range: PA[Int] = RangePA(0, len)
        range.mapSplit(fun(f))
      }
      def tabulateSeg(len: IntRep)(f:IntRep => PA[(A|B)]) = ???

      override def fromArray(arr: Rep[Array[(A|B)]]) = {  //TODO implement using flagCombine
        val len = arr.length
        tabulate(len)(arr(_))
      }

      def empty = ExpSumArray(boolElem.empty, ea.empty, eb.empty)
      def toRep(p: (A|B)) = p fold(l => Left[A,B](ea.toRep(l)), r => Right[A,B](eb.toRep(r)))
    }


  override implicit def parrayElement[A](implicit eA: Elem[A]): Elem[PArray[A]] =
    new PArrayElem[A](eA) with StagedElement[PArray[A]] {
      implicit val elemPA = this
      val segElem = element[(Int,Int)]
      val intElem = element[Int]
      implicit lazy val m:Manifest[PArray[A]] = Manifest.classType(classOf[PArray[A]], ea.manifest)
      implicit lazy val z: Zero[PArray[A]] = Common.zero[PArray[A]](ea.empty)
      implicit def zero = z
      def manifest: Manifest[PArray[A]] = m

      def replicate(count: IntRep, v: PA[A]): PA[PArray[A]] = count match {
        case Def(Const(_)) =>
          val Z = intElem.toRep(0)
          if (count == Z) empty
          else {
            val segLen = v.length
            if (segLen > Z) {
              val arr = ea.replicateSeg(count, v)
              val lens = intElem.replicate(count, segLen)
              val positions = lens.scan
              ExpNestedArray(arr, ExpPairArray(positions, lens))
            }
            else
            {
              //val arr = ea.replicate(Z, ea.toRep(ea.defaultOf))
              val arr = ea.empty
              val segments = segElem.replicate(count, segElem.toRep((0,0)))
              ExpNestedArray(arr, segments)
            }
          }
        case _ => ReplicatePA(count, v)
      }

      def replicateSeg(count: IntRep, v: PA[PArray[A]]): PA[PArray[A]] = ReplicateSegPA(count, v)
//      {
//        if (count == 0) return empty
//        val segLen = v.length
//        if (segLen == 0) {
//          val arr = ea.replicate(0, ea.defaultOf)
//          val segments = segElem.replicate(count, (0, 0))
//          SeqNestedArray(arr, segments)
//        }
//        else {
//          v match {
//            case SeqNestedArray(arr, segs) => {
//              val newArr = ea.replicateSeg(count, arr)
//              val (ps,ls) = unzip(segs)
//              val newLens = intElem.replicateSeg(count, ls)
//              val newps = newLens.scan
//              SeqNestedArray(newArr, SeqPairArray(newps, newLens))
//            }
//            case _ => sys.error("expected SeqNestedArray but found " + v)
//          }
//
//        }
//      }

      def tabulate(len: IntRep)(f:IntRep => PA[A]) = {
        val chunks: Rep[Array[PArray[A]]] = toExp(ArrayTabulate(len, mkLambda(f)))
        val lens = intElem.tabulate(len)(i => chunks(i).length)
        val positions = lens.scan
        val segments = positions.zip(lens)
        val arrToSeg = intElem.tabulateSeg(lens.length)(iSeg => intElem.replicate(lens(iSeg), iSeg))
        var arr = ea.tabulate(arrToSeg.length)(i => {
          val iSeg = arrToSeg(i)
          val Pair(p,l) = segments(iSeg)
          val chunk = chunks(iSeg)
          chunk(i - p)
        })
        ExpNestedArray(arr, segments)
      }
      def tabulateSeg(len: IntRep)(f:IntRep => PA[PArray[A]]) = {
        val temp = toExp(ArrayConcat(ArrayTabulate(len, mkLambda((i:IntRep) => f(i).toArray))))
        tabulate(temp.length)(i => temp(i))
      }

      def empty = ExpNestedArray(ea.empty, segElem.singleton(segElem.toRep((0,0))))
      def toRep(p: PArray[A]) = ???

    }

  override implicit def funcElement[A,B](implicit elema: Elem[A], elemb: Elem[B]): Elem[A => B] =
    new FuncElem[A,B](elema, elemb) with StagedElement[A => B] {
      implicit val elem = this
      lazy val m: Manifest[A=>B] = Manifest.classType(classOf[A=>B], ea.manifest, eb.manifest)
      lazy val unitElem = element[Unit]
      def manifest = m
      lazy val z: Zero[A => B] = Common.zero[A => B](x => eb.zero.zero)
      implicit def zero = z

      def replicate(count: IntRep, v: Rep[A=>B]) = {
        //val paea = element[PArray[A]]
        //val paeb = element[PArray[B]]
        v match {
//          case Def(Lambda(_, _, _)) =>
//            val fun = mkLambda((u: UnitRep) => v)
//            val funl = mkLambda((u: PA[Unit]) => mkLambda((a: PA[A]) => a map { Apply(v, _) })(paea.manifest))
//            ExpFuncArray[Unit, A, B](unitElem.replicate(count, unitElem.toRep(())), fun, funl)
          case Def(clo@Clo(e,fs,fl)) =>
            val c = clo.asInstanceOf[Closure[A,B]]
            implicit val envElem = c.envElem
            ExpFuncArray[c.type#Env, A, B](envElem.replicate(count, c.env), c.clos, c.clol)
          case _ => ReplicatePA(count, v)//???("don't know how to replicate ", v)
        }
      }

      def replicateSeg(count: IntRep, v: PA[A=>B]) = ???

      def tabulate(len: IntRep)(f:IntRep => Rep[A=>B]) = ???
      def tabulateSeg(len: IntRep)(f:IntRep => PA[A=>B]) = ???
      def empty = {
        val clos = mkLambda2((e:Rep[Unit]) => (x:Rep[A]) => eb.zero.zero)
        val clol = mkLambda2((e:PA[Unit]) => (x:PA[A]) => eb.empty)
        ExpFuncArray(element[Unit].empty, clos, clol)
      }
      def toRep(f: A=>B) = Const(f)
    }

  def mkStdArray[A]
          (len : IntRep)
          (func: IntRep => Rep[A])
          (implicit s: Semigroup[A], ea:Elem[A]): PA[A] = {
    implicit val ma = ea.manifest
    val arr = len match {
      case Def(ArrayLength(Def(ArrayTabulate(l, f)))) =>
        //val g = mkLambda(func)
        //var newF = (x: Rep[Int]) => Apply(g, Apply(f, x))
        //val composition = mkLambda(newF)
        ArrayTabulate(l, mkLambda(func))
      case _ => ArrayTabulate(len, mkLambda(func))
    }
    ExpStdArray(arr)
  }

  def mkStdArray[A](len:IntRep, v: Rep[A])
                   (implicit s: Semigroup[A], ea:Elem[A]): PA[A] = {
    implicit val ma = ea.manifest
    val arr = ArrayFill(len, v)
    ExpStdArray(arr)
  }

  def mkUnitArray(len: IntRep): PA[Unit] = ExpUnitArray(len)
  def mkPairArray[A, B](a: PA[A], b: PA[B])(implicit ea: Elem[A], eb: Elem[B], e:Elem[(A,B)]) = ExpPairArray(a, b)

  def mkNestedArray[A](arr: PA[A], segments: PA[(Int, Int)])
                      (implicit ea: Elem[A]): PA[PArray[A]] = ExpNestedArray(arr, segments)

  abstract class StagedArrayBase[A] extends PADef[A] with PArrayBase[A] { self =>
    def zip[B](b: PA[B])(implicit eb: Elem[B], etb: Elem[(A, B)]): PA[(A, B)] = mkPairArray(self, b)
    def sum(implicit m: Monoid[A]): Rep[A] = SumPA(this, m)
    def expandBy[B:Elem](nested: PA[PArray[B]]): PA[A] = nested match {
      case Def(ExpNestedArray(xs, segs)) =>
        this.zip(segs) flatMap { case Pair(x, Pair(_, len)) => elem.replicate(len, x) }
      case _ => ExpandBy(this, nested)
      //case _ => sys.error("expected NestedArray but was " + nested.toString)
    }

    def partition(flags: PA[Boolean]): PA[PArray[A]] = PartitionPA(this, flags)

    override def scan(implicit m: Monoid[A]): PA[A] = ScanPA(this, m)
    def ++(that: PA[A])(implicit epa:Elem[PArray[A]]): PA[A] = Append(this, that)

    // length(this) + length(ifFalse) == length(flags)
    def flagMerge(ifFalse: PA[A], flags: PA[Boolean]) = FlagMerge(this, ifFalse, flags)
    def flagSplit(flags: PA[Boolean]) = FlagSplit(this, flags)
    override def flagCombine(ifFalse: PA[A], flags: PA[Boolean]) = FlagCombine(this, ifFalse, flags)
    override def backPermute(idxs: PA[Int]): PA[A] = BackPermute(this, idxs)
  }

  case class ExpStdArray[T](arr: Rep[Array[T]])(implicit  et: Elem[T])
     extends StagedArrayBase[T]
        with StdArray[T]
  {
    override val elem = et
    implicit val manifest = et.manifest

    def length = arr.length
    def index(i: IntRep) = arr(i)
    def toArray = arr
    def slice(start:IntRep, len:IntRep): PA[T] = et.fromArray(arr.slice(start, start + len))

    def map[B:Elem](f: Rep[T] => Rep[B]) = arr.isConst match {
      case true =>  element[B].tabulate(arr.length)(i => f(arr(i)))
      case _ => MapPA(this, mkLambda(f))
    }

    override def scan(implicit m: Monoid[T]): PA[T] = ExpStdArray(arr.scan)
    override def sum(implicit m: Monoid[T]): Rep[T] = ArraySum(arr, m)
    override def toString = "BaseArray(" + arr.toString + ")"
  }

  case class ExpUnitArray(val len: IntRep) extends StagedArrayBase[Unit] with UnitArray
  {
    override val elem = element[Unit]
    def length = len
    def toArray = ArrayFill(len, toRep(()))
    def map[R:Elem](f: UnitRep => Rep[R]): PA[R] = {
      val v = f(toRep(()))
      element[R].replicate(len, v)
    }

    override def expandBy[B:Elem](nested: PA[PArray[B]]): PA[Unit] = nested match {
      case Def(ExpNestedArray(xs, segs)) =>
        val (_, lens) = unzip(segs)
        ExpUnitArray(lens.sum)
      case _ => ExpandBy(this, nested)
      //case _ => sys.error("expected NestedArray but was " + nested.toString)
    }

    def slice(start: IntRep, l: IntRep) = mkUnitArray(l)
    override def flagCombine(ifFalse: PA[Unit], flags: PA[Boolean]) = this

    // length(this) + length(ifFalse) == length(flags)
    override def flagMerge  (ifFalse: PA[Unit], flags: PA[Boolean]): PA[Unit] = mkUnitArray(flags.length)

    override def flagSplit  (flags: PA[Boolean]) = { // length(this) == length(flags) == (length(A) + length(B))
      val truecount = flags.map((b:BoolRep) => if (b) toRep(1) else toRep(0)).sum
      (mkUnitArray(truecount), mkUnitArray(flags.length - truecount))
    }
    override def backPermute(idxs: PA[Int]): PA[Unit] = ExpUnitArray(idxs.length)
    override def toString = "UnitArray(" + len.toString + ")"
  }

  case class ExpIfArray[A](cond: BoolRep, thenp: PA[A], elsep: PA[A])(implicit val eA: Elem[A])
     extends StagedArrayBase[A]
  {
    override val elem = thenp.elem
    def length = if (cond) thenp.length else elsep.length
    def toArray = if (cond) thenp.toArray else elsep.toArray
    def index(i: IntRep) = if (cond) thenp(i) else elsep(i)
    def slice(start:IntRep, len:IntRep): PA[A] = if (cond) thenp.slice(start, len) else elsep.slice(start, len)

    def map[B:Elem](f: Rep[A] => Rep[B]) = if (cond) thenp.map(f) else elsep.map(f)

    // length(this) + length(ifFalse) == length(flags)
    override def flagMerge(ifFalse: PA[A], flags: PA[Boolean]) =
      if (cond) thenp.flagMerge(ifFalse, flags) else elsep.flagMerge(ifFalse, flags)

    override def flagSplit(flags: PA[Boolean]) = if (cond) thenp.flagSplit(flags) else elsep.flagSplit(flags)
    override def flagCombine(ifFalse: PA[A], flags: PA[Boolean]) =
      if (cond) thenp.flagCombine(ifFalse, flags) else elsep.flagCombine(ifFalse, flags)

    override def backPermute(idxs: PA[Int]): PA[A] =
      if (cond) thenp.backPermute(idxs) else elsep.backPermute(idxs)

    override def toString = "IfArray(" + cond.toString + "," + thenp.toString + "," + elsep.toString + "," + ")"
  }

  case class ExpPairArray[A, B](a: PA[A], b: PA[B])(implicit ea: Elem[A], eb: Elem[B])
     extends StagedArrayBase[(A,B)]
        with PairArray[A,B]
  {
    private def eAB: Elem[(A,B)] = element[(A,B)]
    override val elem = eAB
    implicit val mab = eAB.manifest

    def toArray = {
      val _a = a.toArray; val _b = b.toArray
      val f = (i: IntRep) => Pair(_a(i), _b(i))
      ArrayTabulate(length, mkLambda(f))
    }

    def map[R:Elem](f: Rep[(A,B)] => Rep[R]): PA[R] = //(a,b) match
    {
      //case (Def(Const(_)), Def(Const(_))) =>
        val len = length
        element[R].tabulate(len)(i => f(a(i),b(i)))
      //case _ => MapPA(this, mkLambda(f))
    }

    def slice(start: IntRep, len: IntRep) = ExpPairArray(a.slice(start, len), b.slice(start, len))

    override def flagCombine(ifFalse: PA[(A,B)], flags: PA[Boolean]) = ifFalse match {
      case Def(ExpPairArray(falseA, falseB)) => ExpPairArray(a.flagCombine(falseA, flags), b.flagCombine(falseB, flags))
      case _ => sys.error("ExpPairArray expected by was " + ifFalse.toString)
    }

    // length(this) + length(ifFalse) == length(flags)
    override def flagMerge(ifFalse: PA[(A,B)], flags: PA[Boolean]) = ifFalse match {
      case Def(ExpPairArray(falseA, falseB)) => ExpPairArray(a.flagMerge(falseA, flags), b.flagMerge(falseB, flags))
      case _ => sys.error("ExpPairArray expected by was " + ifFalse.toString)
    }

    override def flagSplit  (flags: PA[Boolean]) = { // length(this) == length(flags) == (length(A) + length(B))
      val Pair(at,af) = a.flagSplit(flags)
      val Pair(bt,bf) = b.flagSplit(flags)
      Pair(ExpPairArray(at,bt), ExpPairArray(af,bf))
    }
    override def backPermute(idxs: PA[Int]): PA[(A,B)] = ExpPairArray(a backPermute idxs, b backPermute idxs)

    override def toString = "PairArray(" + a.toString + "," + b.toString + ")"
  }

  case class ExpSumArray[A,B](val flags: PA[Boolean],  val a: PA[A], val b: PA[B])
                             (implicit ea: Elem[A], eb: Elem[B])
    extends StagedArrayBase[(A|B)]
       with SumArray[A,B]
  {
    private def eAB: Elem[(A|B)] = element[(A|B)]
    override val elem = eAB
    implicit val mab = eAB.manifest

    lazy val indices: PA[Int] = {
      val aindices = flags.map((b: BoolRep) => if (b) 0 else 1).scan
      val bindices = flags.map((b: BoolRep) => if (b) 1 else 0).scan
      bindices.flagCombine(aindices, flags)
    }

    def index(i: IntRep) = if (flags(i)) Right[A,B](b(indices(i))) else Left[A,B](a(indices(i)))
    override def toArray = ArrayTabulate(flags.length, fun { index(_) })

    def map[R:Elem](f: Rep[(A|B)] => Rep[R]): PA[R] = {
      val len = length
      element[R].tabulate(len)(i => f(index(i)))
    }

    def slice(start: IntRep, len: IntRep) = {
      val fslice = flags.slice(start, len)
      val islice = indices.slice(start, len)
      val Pair(ib, ia) = islice.flagSplit(fslice)
      ExpSumArray(fslice, sliceByIndices(a, ia), sliceByIndices(b, ib))
    }

    // length(this) + length(ifFalse) == length(flags)
    override def flagMerge(ifFalse: PA[(A|B)], fs: PA[Boolean]) = ???
//      ifFalse matchType {
//      (arr: SeqSumArray[A,B]) => {
//        val newFlags = this.flags.flagMerge(arr.flags, fs)
//        val newIndices = this.indices.flagMerge(arr.indices, fs)
//        fs.zip(newFlags).zip(newIndices) map {
//          case ((isTrueArr,isB),i) =>
//            if (isTrueArr) {
//              if (isB) Right(b(i)) else Left(a(i))
//            }
//            else {
//              if (isB) Right(arr.b(i)) else Left(arr.a(i))
//            }
//        }
//      }
//    }

    // length(this) == length(flags) == (length(A) + length(B))
    override def flagSplit  (fs: PA[Boolean]) = {
      val Pair(trueFlags, falseFlags) = this.flags.flagSplit(fs)
      val Pair(trueIndices, falseIndices) = this.indices.flagSplit(fs)
      val trueArr: PA[(A|B)] = trueFlags.zip(trueIndices) map { case Pair(isB,i) => if (isB) Right[A,B](b(i)) else Left[A,B](a(i)) }
      val falseArr: PA[(A|B)] = falseFlags.zip(falseIndices) map { case Pair(isB,i) => if (isB) Right[A,B](b(i)) else Left[A,B](a(i)) }
      (trueArr, falseArr)
    }

    private def sliceByIndices[T:Elem](arr:PA[T], indices: PA[Int]): PA[T] = {
      val len = indices.length
      if (len == 0) return element[T].empty
      val first = indices(0)
      arr.slice(first, len)
    }
  }

  case class ExpNestedArray[A](val arr: PA[A], val segments: PA[(Int,Int)])
         (implicit ea: Elem[A], epa: Elem[PArray[A]])
          extends StagedArrayBase[PArray[A]] with NestedArray[A]
  {
    val intElem = element[Int]
    override val elem = epa
    def toArray = ???
    def map[R:Elem](f: PA[A] => Rep[R]): PA[R] = {
      val len = length
      element[R].tabulate(len)(i => {val Pair(p,l) = segments(i); f(arr.slice(p,l))})
    }

    def slice(start: IntRep, len: IntRep): PA[PArray[A]] = {
      val Z = toRep(0)
      if (len == Z) elem.empty
      else {
        val (positions, lens) = unzip(segments.slice(start, len))
        val arrPos = positions(Z)
        val arrLen = positions(len-toRep(1)) + lens(len-toRep(1)) - arrPos
        val newArr = arr.slice(arrPos, arrLen)
        val newPositions: PA[Int] = positions.zip(intElem.replicate(len, arrPos)).map {case Pair(p, ofs) => p - ofs}
        ExpNestedArray(newArr, newPositions.zip(lens))
      }
    }

    override def flagMerge(ifFalse: PA[PArray[A]], fs: PA[Boolean]) = ifFalse match {
      case Def(ExpNestedArray(a, segs)) => {
        val (thisPositions, thisLens) = unzip(this.segments)
        val (thatPositions, thatLens) = unzip(segs)
        val newLens = thisLens.flagMerge(thatLens, fs)
        val newPositions = thisPositions.flagMerge(thatPositions, fs)
        fs.zip(newPositions.zip(newLens)) map {
          case Pair(flag,Pair(p,l)) => if (flag) this.arr.slice(p,l) else a.slice(p,l)
        }
      }
    }

    override def flagSplit  (fs: PA[Boolean]): Rep[(PArray[PArray[A]], PArray[PArray[A]])] = {
      val (positions, lens) = unzip(this.segments)
      val Pair(posTrue,posFalse) = positions.flagSplit(fs)
      val Pair(lensTrue,lensFalse) = lens.flagSplit(fs)
      val arrTrue = posTrue.zip(lensTrue) map {case Pair(p,l) => this.arr.slice(p,l)}
      val arrFalse = posFalse.zip(lensFalse) map {case Pair(p,l) => this.arr.slice(p,l)}
      Pair(arrTrue, arrFalse)
    }
    override def backPermute(idxs: PA[Int]): PA[PArray[A]] = {
      ???
      //val segs = segments backPermute idxs
      //arr.sliceSegments(segs)
    }
    override def toString = "NArray(" + arr.toString + "," + segments.toString + ")"
  }

  case class ExpFuncArray[E, A, B](env: PA[E],
                                   fun: Rep[E => A => B],
                                   funl: Rep[PArray[E] => PArray[A] => PArray[B]])
                                  (implicit val ee: Elem[E], ea: Elem[A], eb: Elem[B], ef:Elem[A => B])
     extends StagedArrayBase[A => B]
        with FuncArray[E,A,B]
  {
    override val elem = ef
    def index(i: IntRep) = Apply(fun, env.index(i))
    def toArray = ???
    def slice(start:IntRep, len:IntRep) = ExpFuncArray(env.slice(start, len), fun, funl)

    def map[C:Elem](f: Rep[A => B] => Rep[C]) = env map { e => f(Apply(fun, e)) }

    // length(this) + length(ifFalse) == length(flags)

    override def flagSplit(flags: PA[Boolean]) = {
      val Pair(fa, fb) = env flagSplit(flags)
      Pair(ExpFuncArray(fa, fun, funl), ExpFuncArray(fb, fun, funl))
    }

    override def backPermute(idxs: PA[Int]): PA[A => B] = ExpFuncArray(env backPermute idxs, fun, funl)
  }
  def mkFuncArray[E, A, B](env: PA[E],
                           fun: Rep[E => A => B],
                           funl: Rep[PArray[E] => PArray[A] => PArray[B]])
                          (implicit ee: Elem[E], ea: Elem[A], eb: Elem[B], ef:Elem[A => B]): PA[A=>B] =
    ExpFuncArray(env, fun, funl)

  case class ExpBinopArray[A:Elem]
                (op: BinOp[A], lhs: PA[A], rhs: PA[A])
     extends StagedArrayBase[A]
        with BinopArray[A]
  {
    override lazy val elem = element[A]
    def index(i: IntRep) = op.copyWith(lhs(i), rhs(i))
    def toArray = ???
    def slice(start:IntRep, len:IntRep) = ExpBinopArray(op, lhs.slice(start, len), lhs.slice(start, len))

    def map[C:Elem](f: Rep[A] => Rep[C]) = (lhs zip rhs) map { case Pair(l,r) => f(op.copyWith(l,r)) }

    // length(this) + length(ifFalse) == length(flags)
    override def flagMerge(ifFalse: PA[A], flags: PA[Boolean]) = ???
    override def flagSplit(flags: PA[Boolean]) = ???
    override def flagCombine(ifFalse: PA[A], flags: PA[Boolean]) = ???
    override def backPermute(idxs: PA[Int]): PA[A] = ExpBinopArray(op, lhs backPermute idxs, rhs backPermute idxs)
  }

  abstract class ExpStubArray[A:Elem]
     extends StagedArrayBase[A]
        with StubArray[A]
  {
    override lazy val elem = element[A]
    implicit def mA = elem.manifest

    protected def createStub(source: PA[A]): PA[A] = {
      var eA = element[A]
      eA match {
        case pe: PairElem[_,_] =>
          val peAny = pe.asInstanceOf[PairElem[Any,Any]]
          val ab = pimpPairArray[Any,Any](source.asInstanceOf[PA[(Any,Any)]])(peAny.ea, peAny.eb)
          val a = ab.fst
          val b = ab.snd
          val eAB = pairElement(peAny.ea, peAny.eb)
          mkPairArray(a, b)(peAny.ea, peAny.eb, eAB).asInstanceOf[PA[A]]
        case ae: PArrayElem[_] => source
        case _ => source
      }
    }

    def arr = createStub(this)
    def length = LengthPA(arr)
    def index(i: IntRep) = IndexPA(arr, i)
    def toArray = elem match {
      case _: BaseElem[_] => ToArray(arr)
      case _ => ToArray(arr)
    }
    def slice(start:IntRep, len:IntRep) = SlicePA(arr, start, len)
    def map[C:Elem](f: Rep[A] => Rep[C]) = {
      val stub = arr
      stub match {
        case Def(_: PairArray[_,_]) =>
          stub map f
        case _ =>
          val lam = mkLambda(f)
          MapPA(arr, lam)
      }
    }
  }

  case class VarPA[A:Elem](a: PA[A]) extends ExpStubArray[A] {
    override def arr = createStub(a)
  }
  case class LengthPA[A](arr: PA[A]) extends Def[Int]
  case class IndexPA[A](arr: PA[A], i: IntRep) extends Def[A]
  case class SumPA[A](arr: PA[A], implicit val m: Monoid[A]) extends Def[A]
  case class ScanPA[A:Elem](xs: PA[A], implicit val m: Monoid[A]) extends ExpStubArray[A]
  case class ToArray[A](arr: PA[A])(implicit val eA: Elem[A]) extends ArrayDef[A]

  // generates len elements: x1 = m.zero, x2 = x1 + step, x3 = x2 + step, ...
  case class GeneratePA[A:Elem](len: Rep[Int], step: Rep[A], implicit val m: Monoid[A]) extends ExpStubArray[A] {
    //def arr = this
  }
  case class ReplicatePA[A:Elem](count: IntRep, v: Rep[A]) extends ExpStubArray[A] {
    //def arr = this
  }
  case class ReplicateSegPA[A](count: IntRep, v: PA[A])(implicit val eA: Elem[A]) extends ExpStubArray[A]
  case class ApplyPA[A:Elem, B:Elem](fs: PA[A=>B], args: PA[A]) extends ExpStubArray[B] {
    //def arr = this
  }
  case class ExpandBy[A:Elem,B](source: PA[A], nested: PA[PArray[B]]) extends ExpStubArray[A] {
    //def arr = this
  }
  case class PartitionPA[A:Elem](source: PA[A], flags: PA[Boolean]) extends ExpStubArray[PArray[A]] {
    //def arr = this
  }
  case class SlicePA[A:Elem](source: PA[A], start:IntRep, len:IntRep) extends ExpStubArray[A] {
    //def arr = this
  }
  case class MapPA[A,B:Elem](source: PA[A], lam: Rep[A => B]) extends ExpStubArray[B] {
    //def arr = this
  }
  case class IndexLiftedPA[A:Elem](source: PA[PArray[A]], indexes: PA[Int]) extends ExpStubArray[A] {
    //def arr = this
  }
  case class SumLiftedPA[A:Elem](source: PA[PArray[A]], m: Monoid[A]) extends ExpStubArray[A] {
    //def arr = this
  }
  case class MapLiftedPA[A:Elem,B:Elem](source: PA[PArray[A]], lam: PA[A => B]) extends ExpStubArray[PArray[B]] {
    //def arr = this
  }

  case class FlagMerge[A:Elem](ifTrue: PA[A], ifFalse: PA[A], flags: PA[Boolean]) extends ExpStubArray[A] {
    //def arr = this
  }
  case class FlagSplit[A:Elem](arr: PA[A], flags: PA[Boolean]) extends Def[(PArray[A], PArray[A])]
  case class FlagCombine[A:Elem](ifTrue: PA[A], ifFalse: PA[A], flags: PA[Boolean]) extends ExpStubArray[A] {
    //def arr = this
  }

  case class Append[T:Elem](xs: PA[T], that: PA[T]) extends ExpStubArray[T] {
    //def arr = this
  }

  case class BackPermute[T:Elem](x: PA[T], idxs: PA[Int]) extends ExpStubArray[T] {
    //def arr = this
  }
  case class LShift[T:Elem](x: PA[T], step: Rep[Int], fillWith: Rep[T]) extends ExpStubArray[T] {
    //def arr = this
  }
  case class RShift[T:Elem](x: PA[T], step: Rep[Int], fillWith: Rep[T]) extends ExpStubArray[T] {
    //def arr = this
  }
  case class NestArrays[A:Elem](a1: PA[A], a2: PA[A]) extends ExpStubArray[PArray[A]] {
    //def arr = this
  }

  override def mirror[A](d: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[_] = d match {
    case x@ExpIfArray(c, t, e) => {
      implicit val elem = x.eA; ExpIfArray(f(c), f(t/*.as[PArray[A]]*/), f(e/*.as[PArray[A]]*/))
    }//(eA.asInstanceOf[Elem[Any]])
    case _ => super.mirror(d, f)
  }

  override def rewrite[T](d: Def[T])(implicit eT: Elem[T]) = d match {
    case ExpIfArray(Def(Const(x)), thenp, elsep) =>
      x match { case true => thenp case _ => elsep }
    case ExpStdArray(Def(ToArray(arr))) => arr
    case ToArray(Def(ExpStdArray(arr))) => arr
    case NestedArrayValues(Def(ExpNestedArray(arr, _))) => arr
      
    case _ => super.rewrite(d)
  }
}

trait ScalaGenPArrays extends ScalaGenEffect { this: StagedImplementation =>

  abstract override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case ExpStdArray(arr) => emitValDef(sym, "StdArray(" + quote(arr) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}



