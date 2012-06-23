package scalan.tests

import org.junit.Test
import scala.{Left => L, Right => R}
import scalan.staged.{StagedOption, ScalanStaged, ScalanExportGraph}

class StagedOptionTests extends TestContext {
  override val scalan = new ScalanStaged with ScalanExportGraph with StagedOption
  import scalan._
  val prefix = "test-out/StagedOptionTests/"

  @Test def replicateOptionTests() {
    val name = "option_"
    withEmitException(name) {
      val opt: Option[Int] = Some(10)
      val replicatedSome = replicate(2, opt)
      val Def(ExpViewArray(_, iso1)) = replicatedSome
      emitDepGraph(List(replicatedSome,iso1.toFunTo, iso1.toFunFrom), prefix + name + "replicatedSome.dot", true)

      val replicatedNone = replicate(2, None: Option[Int])
      val Def(ExpViewArray(_, iso2)) = replicatedNone
      emitDepGraph(List(replicatedNone,iso2.toFunTo, iso2.toFunFrom), prefix + name + "replicatedNone.dot", false)

      val zipped = replicatedNone zip replicatedNone
      val Def(ExpViewArray(_, iso3)) = zipped
      emitDepGraph(List(zipped/*,iso3.toFunTo, iso3.toFunFrom*/), prefix + name + "zipped.dot", false)

      val opt2: Option[Option[Int]] = Some(None: Option[Int])
      val replicatedNone2 = replicate(2, opt2)
      val Def(ExpViewArray(_, iso4)) = replicatedNone2
      emitDepGraph(List(replicatedNone2,iso4.toFunTo, iso4.toFunFrom), prefix + name + "replicatedNone2.dot", false)
    }
  }

}

//object T extends StagedSumTests
//import T._
//import scalan._