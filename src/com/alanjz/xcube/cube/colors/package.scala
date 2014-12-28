package com.alanjz.xcube.cube

package object colors {

  trait Color

  sealed abstract class FacetColor(val name : String, chr : Char, n : Int) extends Color {
    def toChar = chr
    def toInt = n
    override def toString = name
  }

  case object Blue extends FacetColor("blue", 'B', 0)
  case object Red extends FacetColor("red", 'R', 1)
  case object White extends FacetColor("white", 'W', 2)
  case object Orange extends FacetColor("orange", 'O', 3)
  case object Green extends FacetColor("green", 'G', 4)
  case object Yellow extends FacetColor("yellow", 'Y', 5)

}
