package scalan.util

import scala.text._
import Document._

object DocumentExtensions {
  val ED: scala.text.Document = empty

  class DocumentOps(d: Document) {
    def formatToString: String = {
      val writer = new java.io.StringWriter()
      d.format(120, writer)
      writer.toString
    }
  }
  implicit def pimpDocument(d: Document): DocumentOps = new DocumentOps(d)
}
