package scalan.samples

import scalan.dsl._

trait HelloScalan extends Scalan {
  def Hello(names: Array[String]) = {
    fromArray(toRep(names)) map {name => toRep("Hello, ") + name + toRep("!")} toArray
  }
}

import scalan.sequential._

//object Sample extends HelloScalan with ScalanSequential {
//
//}

// scala> import scalan.samples._
// scala> MySample.HelloScalan(Array("Alex", "Ilya"))
