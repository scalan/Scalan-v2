package scalan.samples

import scalan.staged.{ScalanExportGraph, ScalanStaged}
import scala.virtualization.lms.common.{Base}
import scala.virtualization.lms.internal.Expressions
import scalan.dsl.Scalan
import scalan.common.{Zero, Semigroup}

trait Generics {
  trait Elem[A]
  implicit object EInt extends Elem[Int]
  implicit object EChar extends Elem[Char]

  case class EPair[A,B](ra: Elem[A], rb: Elem[B]) extends Elem[(A,B)]
  implicit def elemPair[A,B](implicit ra:Elem[A], rb: Elem[B]): Elem[(A,B)] = EPair(ra,rb)

  def serialize[A](x:A)(implicit r:Elem[A]): String = (x,r) match {
    case (i: Int, EInt) => serializeInt(i)
    case (c: Char, EChar) => serializeChar(c)
    case ((a, b), EPair(ra, rb)) => "(%s, %s)".format(serialize(a)(ra), serialize(b)(rb))
  }

  def serializeInt(i: Int): String = i.toString
  def serializeChar(c: Char): String = "'%s'".format(c.toString)
}

object G extends Generics

trait GenericsExp extends ScalanStaged with ScalanExportGraph {
  //def infix_format(s: Exp[String], args: Exp[Any]*) = StringFormat(s, args.toList)
  implicit def addStringExtensions(s: Rep[String]) = new {
    def format(args: Exp[Any]*) = StringFormat(s, args.toList)
  }
  implicit lazy val charElement:   Elem[Char] =
    new StagedBaseElement[Char]()(implicitly[Semigroup[Char]], Zero.CharZero, manifest[Char])

  val EInt = element[Int]
  val EChar = element[Char]
  object EPair {
    def unapply[A](e: Elem[A]): Option[(Elem[Any], Elem[Any])] =  e match {
      case pe: PairElem[Any,Any] =>
        Some((pe.ea, pe.eb))
      case _ =>
        None
    }
  }
  //  trait El[A]
//  implicit object EInt extends El[Int]
//  implicit object EChar extends El[Char]
//
//  case class EPair[A,B](ra: El[A], rb: El[B]) extends El[(A,B)]
//  implicit def elemPair[A,B](implicit ra:El[A], rb: El[B]): El[(A,B)] = EPair(ra,rb)

  def serialize[A](x:Rep[A])(implicit r:Elem[A]): Rep[String] = r match {
    case EInt => serializeInt(x.as[Int])
    case EChar => serializeChar(x.as[Char])
    case EPair(ra, rb) =>
      val Pair(a, b) = x
      toRep("(%s, %s)").format(serialize[Any](a)(ra), serialize[Any](b)(rb))
  }

  def serializeInt(i: Rep[Int]): Rep[String] = SerializeInt(i)
  def serializeChar(c: Rep[Char]): Rep[String] = SerializeChar(c)

  case class SerializeInt(i: Rep[Int]) extends Def[String]
  case class SerializeChar(c: Rep[Char]) extends Def[String]
  case class StringFormat(f: Rep[String], args: List[Rep[Any]]) extends Def[String]

}

object GE extends GenericsExp {
  val prefix = "test-out/GenericsExp/"

  def test() {
    val name = "serialize_"
    val x = 10
    val y = 's'
    val p = Pair(x, y)
    val r1 = serialize(p)

    emitDepGraph(List(r1), prefix + name + "r1.dot", true)

    val pp = Pair(p, p)
    val r2 = serialize(pp)
    emitDepGraph(List(r2), prefix + name + "r2.dot", true)
  }

}