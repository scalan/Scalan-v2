package scala.virtualization.lms
package common

import internal.GraphVizExport
import scalan.dsl.ArraysBase

trait ExportGraph extends GraphVizExport { self: ArraysBase =>
  
  def exportGraph(file: String, landscape: Boolean)(x: Exp[Any]) =
    emitDepGraph(x, "test2-fft2-dot", landscape)
  
}
