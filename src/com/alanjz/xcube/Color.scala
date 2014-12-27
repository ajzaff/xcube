package com.alanjz.xcube

sealed abstract class Color(val name : String, chr : Char, n : Int) {
  def toChar = chr
  def toInt = n
  override def toString = name
}

object Color {
  implicit def toInt(lhs : Color) = lhs.toInt

  case object Blue extends Color("blue", 'B', 0)
  case object Red extends Color("red", 'R', 1)
  case object White extends Color("white", 'W', 2)
  case object Orange extends Color("orange", 'O', 3)
  case object Green extends Color("green", 'G', 4)
  case object Yellow extends Color("yellow", 'Y', 5)
}