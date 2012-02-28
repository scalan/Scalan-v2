package scalan.staged

import scalan.dsl.ScalanContext
//import scalan.sequential.{SeqViewScalan, SeqViewContext}


trait StagedContext extends ScalanContext {
  val scalan: ScalanStaged //with SeqViewScalan

}
