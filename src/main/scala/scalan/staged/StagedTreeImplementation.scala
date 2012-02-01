package scalan.staged

import collection.generic.CanBuildFrom
import scala.reflect.Manifest
import scalan.common._
import Common._
import scalan.util.Utils
import Utils._
import scalan.dsl._

trait StagedTreeImplementation extends PTrees with StagedImplementation with Arrays {

//  implicit def toTree[A](item: Rep[Item[A]]): Rep[Tree[A]] = { val (v,c) = item; new Tree(v,c) }
//
//  implicit def treeElement[A](implicit ea: Elem[A]): Elem[Tree[A]] = new StagedElement[Tree[A]] {
//    implicit val elemTA:Elem[Tree[A]] = this
//    implicit val ma: Manifest[A] = ea.manifest
//    implicit val m: Manifest[Tree[A]] = Manifest.classType(classOf[Tree[A]], ea.manifest)
//
//    def itemElem: Elem[Item[A]] = element[Item[A]]
//    def manifest: Manifest[Tree[A]] = m
//
//    def replicate(count: Rep[Int], v: Rep[Tree[A]]): PA[Tree[A]] = {
//      if (count == 0) return empty
//      val item = (v.value, v.children)
//      val items = itemElem.replicate(count, item)
//      toTreeArray(items)
//    }
//
//    def replicateSeg(count: Rep[Int], v: PA[Tree[A]]): PA[Tree[A]] = {
//      if (count == 0 || v.length == 0) return empty
//      val items = toItems(v).get
//      toTreeArray(itemElem.replicateSeg(count, items))
//    }
//
//
//    def tabulate(len: Rep[Int])(f: Rep[Int] => Rep[Tree[A]]) = {
//      if (len == 0) empty
//      else {
//        val items = itemElem.tabulate(len)(i => {val t = f(i); (t.value, t.children)})
//        if (items.length == 0) empty
//        else toTreeArray(items)
//      }
//    }
//
//    def tabulateSeg(len: Rep[Int])(f: Rep[Int] => PA[Tree[A]]) = {
//      if (len == 0) empty
//      else {
//        val items = itemElem.tabulateSeg(len)(i => toItems(f(i)) some {items => items} none itemElem.empty)
//        if (items.length == 0) empty
//        else toTreeArray(items)
//      }
//    }
//    def empty = SeqTreeArray(None)
//  }
//
//  def toItems[A](ts: PA[Tree[A]]):Option[PA[Item[A]]] = ts matchType { (arr: SeqTreeArray[A]) => arr.items }
//  def toItems[A](ts: PA[Tree[A]], defItems: => PA[Item[A]]): PA[Item[A]] = toItems(ts) some {items => items} none defItems
//  def toTreeArray[A](items: PA[Item[A]])
//                    (implicit ea: Elem[A]):PA[Tree[A]] = SeqTreeArray(Some(items))
//
//  case class SeqTreeArray[A](items: Option[PA[Item[A]]])
//         (implicit ea: Elem[A], eta: Elem[Tree[A]])
//          extends TreeArray[A](items)
//          with SeqPArray[Tree[A]]
//  {
//    override val elem = eta
//    override val elem2 = items ? element[(Tree[A],Tree[A])] | null
//    implicit val ma: Manifest[A] = ea.manifest
//    implicit val bfa: CanBuildFrom[Array[Array[A]], A, Array[A]] = Array.canBuildFrom[A]
//
//    def map[R:Elem](f: Rep[Tree[A]] => Rep[R]): PA[R] = {
//      val len = length
//      element[R].tabulate(len)(i => {val t = this(i); f(t)})
//    }
//
//    def slice(start: Rep[Int], len: Rep[Int]): PA[Tree[A]] = {
//      if (len == 0) return elem.empty
//      items some { (is :PA[Item[A]]) => toTreeArray(is.slice(start, len))} none {sys.error("Cannot slice empty TreeArray " + start + " " + len)}
//    }
//
//    def flagMerge(ifFalse: PA[Tree[A]], fs: PA[Boolean]) = {
//      val emptyItems = element[Item[A]].empty
//      val thisItems = toItems(this, emptyItems)
//      val thatItems = toItems(ifFalse, emptyItems)
//      toTreeArray(thisItems flagMerge(thatItems, fs))
//    }
//
//    def flagSplit(fs: PA[Boolean]) = {
//      val emptyItems = element[Item[A]].empty
//      val thisItems = toItems(this, emptyItems)
//      val (tItems, fItems) = thisItems.flagSplit(fs)
//      (toTreeArray(tItems), toTreeArray(fItems))
//    }
//  }
}
