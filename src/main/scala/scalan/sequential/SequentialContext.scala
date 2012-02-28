package scalan.sequential

import scalan.dsl.{Scalan, ScalanContext}

//trait SeqViewScalan extends Scalan {
//  //type Iso[A,B] = IsoBase[A,B]
//}
//
//trait SeqViewContext extends ScalanContext {
//  val scalan: SeqViewScalan
//}

trait SequentialContext extends ScalanContext {
  val scalan: ScalanSequential

}
