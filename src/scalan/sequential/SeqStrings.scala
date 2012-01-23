package scalan.sequential

import scalan.dsl.{ArraysBase, ScalanStrings}

trait SeqStrings extends ScalanStrings { self: ArraysBase =>
  //implicit def pimpString(ss: Rep[String]): ScalanStringOps#Ops = sys.error("should not be called")
  def string_split(s: Rep[String], separators: Rep[String]): Rep[Array[String]] = !!! //s.asInstanceOf[String].split(separators)

  def string_trim(s: Rep[String]): Rep[String] = !!! //s.asInstanceOf[String].trim()

  def string_plus(s: Rep[Any], o: Rep[Any]): Rep[String] = !!!
}
