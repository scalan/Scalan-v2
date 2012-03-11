package scalan.tests

import scalan.samples._
import org.junit.Test

class TypeClassesTests extends TestContext
                          with TypeClasses
{
  import scalan._

  val prefix = "test-out/TypeClassesTests/"


  @Test def typeclassTests() {
    val name = "typeclass"
    withEmitException(name) {
      val arr1 = replicate(3, 10)
      val arr2 = replicate(3, 20)

      val res1 = vectSum(arr1, arr2)
      emitDepGraph(List(res1), prefix + name + "_vectSum.dot", false)

      val res2 = vectSum(arr1, arr2)(intRepMonoidMul, element[Int])
      emitDepGraph(List(res2), prefix + name + "_vectMul.dot", false)

      val ps1 = replicate(3, Point(10,10))
      val ps2 = replicate(3, Point(20,20))
      val res3 = vectSum(ps1, ps2)
      emitDepGraph(List(res3), prefix + name + "_point_vectSum.dot", false)
    }
  }

}
