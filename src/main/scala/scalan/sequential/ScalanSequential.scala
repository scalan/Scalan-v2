package scalan.sequential

import reflect.Manifest
import virtualization.lms.ppl.ScalaOpsPkg
import java.io.{BufferedReader, FileReader}
import scalan.dsl.{ScalanBase, Scalan}

trait SeqOpsPkg extends ScalaOpsPkg with SeqImplBase {
  def implicit_convert[X, Y](x: Rep[X])(implicit c: (X) => Y): Rep[Y] = !!!

  def fractional_divide[T](lhs: Rep[T], rhs: Rep[T])(implicit f: Fractional[T], et: Elem[T]): Rep[T] = !!!

  def ordering_min[T](lhs: Rep[T], rhs: Rep[T])(implicit n: Ordering[T], et: Elem[T]): Rep[T] = !!!

  def ordering_max[T](lhs: Rep[T], rhs: Rep[T])(implicit n: Ordering[T], et: Elem[T]): Rep[T] = !!!

  def ordering_equiv[T](lhs: Rep[T], rhs: Rep[T])(implicit n: Ordering[T]): Rep[Boolean] = !!!

  def ordering_gteq[T](lhs: Rep[T], rhs: Rep[T])(implicit n: Ordering[T]): Rep[Boolean] = !!!

  def ordering_gt[T](lhs: Rep[T], rhs: Rep[T])(implicit n: Ordering[T]): Rep[Boolean] = !!!

  def ordering_lteq[T](lhs: Rep[T], rhs: Rep[T])(implicit n: Ordering[T]): Rep[Boolean] = !!!

  def ordering_lt[T](lhs: Rep[T], rhs: Rep[T])(implicit n: Ordering[T]): Rep[Boolean] = !!!

  //override def exit(status: Rep[Int]): Rep[Nothing] = !!!

  def println(x: Rep[Any]): Rep[Unit] = scala.Predef.println(x)

  def print(x: Rep[Any]): Rep[Unit] = !!!

  def range_foreach(r: Rep[Range], f: (Rep[Int]) => Rep[Unit]): Rep[Unit] = !!!

  def range_end(r: Rep[Range]): Rep[Int] = !!!

  def range_step(r: Rep[Range]): Rep[Int] = !!!

  def range_start(r: Rep[Range]): Rep[Int] = !!!

  def range_until(start: Rep[Int], end: Rep[Int]): Rep[Range] = !!!

  def obj_fr_apply(s: Rep[String]): Rep[FileReader] = !!!

  def br_close(b: Rep[BufferedReader]): Rep[Unit] = !!!

  def br_readline(b: Rep[BufferedReader]): Rep[String] = !!!

  def obj_br_apply(f: Rep[FileReader]): Rep[BufferedReader] = !!!

  def numeric_toFloat[T](lhs: Rep[T])(implicit n: Numeric[T]): Rep[Float] = !!!

  def numeric_times[T](lhs: Rep[T], rhs: Rep[T])(implicit n: Numeric[T], et: Elem[T]): Rep[T] = n.times(lhs, rhs)

  def numeric_minus[T](lhs: Rep[T], rhs: Rep[T])(implicit n: Numeric[T], et: Elem[T]): Rep[T] = !!!

  def numeric_plus[T](lhs: Rep[T], rhs: Rep[T])(implicit n: Numeric[T], et: Elem[T]): Rep[T] = !!!
}

trait ScalanSequential
  extends Scalan
  with SeqImplBase
  with SeqImplementation
  with SeqViews
  with SeqTreeImplementation
  with SeqStrings
  with SeqArithmetic
  with SeqLogical
  with SeqStdArrayOps
  with SeqPArrayOps
  with SeqOpsPkg
{

  implicit def paManifest[A](implicit ea: Elem[A]): Manifest[PArray[A]] =
      Manifest.classType(classOf[PArray[A]], ea.manifest)

}
