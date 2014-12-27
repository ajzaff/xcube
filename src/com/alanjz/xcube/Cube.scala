package com.alanjz.xcube

import com.alanjz.xcube.Color._

/*
             R  R  R
             R  R  R
             R  R  R
    B  B  B  W  W  W  G  G  G  Y  Y  Y
    B  B  B  W  W  W  G  G  G  Y  Y  Y
    B  B  B  W  W  W  G  G  G  Y  Y  Y
             O  O  O
             O  O  O
             O  O  O

             09 18 27
             10 19 28
             11 20 29
    00 03 06 12 21 30 36 39 42 45 48 51
    01 04 07 13 22 31 37 40 43 46 49 52
    02 05 08 14 23 32 38 41 44 47 50 53
             15 24 33
             16 25 34
             17 26 35

   */

class Cube(protected val cells : List[Color],
           val moves : List[Move] = List.empty) {

  assert(cells.length == 54)
  //assert(cells.forall(_ != null))

  private lazy val _solved = this.equals(Cube.solved)
  private lazy val _hashCode = {
    var result = 1
    for (e <- cells if e != null)
      result = 31 * result + e
    result
  }
  private lazy val _str = {
    val builder = StringBuilder.newBuilder
    for(i <- 9 to 11) {
      builder ++= s"      ${cells(i).toChar} ${cells(i+9).toChar} ${cells(i+18).toChar}\n"
    }
    val row1 = StringBuilder.newBuilder
    val row2 = StringBuilder.newBuilder
    val row3 = StringBuilder.newBuilder
    for(i <- Seq(0,3,6,12,21,30,36,39,42,45,48,51)) {
      row1 ++= s"${cells(i).toChar} "
      row2 ++= s"${cells(i+1).toChar} "
      row3 ++= s"${cells(i+2).toChar} "
    }
    builder ++= row1.mkString + '\n'
    builder ++= row2.mkString + '\n'
    builder ++= row3.mkString + '\n'
    for(i <- 15 to 17) {
      builder ++= s"      ${cells(i).toChar} ${cells(i+9).toChar} ${cells(i+18).toChar}\n"
    }
    builder ++= s"moves=${moves.mkString(" ")}\n"
    builder ++= s"hash=$hashCode\n"
    builder ++= s"legal=$isLegal\n"
    builder ++= s"solved=$isSolved\n"
    builder.mkString
  }
  private lazy val _legal : Boolean = {
    val counts = Array.fill[Int] (6) (0)
    for(i <- cells) {
      counts(i) += 1
    }
    counts.forall(_==9)
  }

  def isSolved : Boolean = _solved

  def isLegal : Boolean = _legal

  def make(move : Move, trace : Boolean = true) : Cube = {
    if(move == null) throw new NullPointerException()
    move match {
      case Move.U => doU(trace)
      case Move.UPrime => undoU(trace)
      case Move.D => doD(trace)
      case Move.DPrime => undoD(trace)
      case Move.F => doF(trace)
      case Move.FPrime => undoF(trace)
      case Move.B => doB(trace)
      case Move.BPrime => undoB(trace)
      case Move.L => doL(trace)
      case Move.LPrime => undoL(trace)
      case Move.R => doR(trace)
      case Move.RPrime => undoR(trace)
    }
  }

  def unmake(move : Move, trace : Boolean = true) : Cube = make(move.inverse, trace)

  private def doU(trace : Boolean) : Cube = {

    val array : Array[Color] = cells.toArray

    val temp0 = array(0)
    val temp1 = array(3)
    val temp2 = array(6)

    // move face 1 to face 0.
    array(0) = array(12)
    array(3) = array(21)
    array(6) = array(30)

    // move face 2 to face 1.
    array(12) = array(36)
    array(21) = array(39)
    array(30) = array(42)

    // move face 3 to face 2.
    array(36) = array(45)
    array(39) = array(48)
    array(42) = array(51)

    // move face 0 to face 3.
    array(45) = temp0
    array(48) = temp1
    array(51) = temp2

    // rotate the face.
    val t0 = array(9)
    val t1 = array(10)
    val t2 = array(11)
    array(10) = array(20)
    array(11) = array(29)
    array(20) = array(28)
    array(29) = array(27)
    array(28) = array(18)
    array(27) = t0
    array(18) = t1
    array(9) = t2

    new Cube(array.toList, if(trace) moves :+ Move.U else moves)
  }

  private def undoU(trace : Boolean) : Cube = {
    val array : Array[Color] = cells.toArray

    val temp0 = array(45)
    val temp1 = array(48)
    val temp2 = array(51)

    // move face 2 to face 3.
    array(45) = array(36)
    array(48) = array(39)
    array(51) = array(42)

    // move face 1 to face 2.
    array(36) = array(12)
    array(39) = array(21)
    array(42) = array(30)

    // move face 3 to face 2.
    array(12) = array(0)
    array(21) = array(3)
    array(30) = array(6)

    // move face 0 to face 3.
    array(0) = temp0
    array(3) = temp1
    array(6) = temp2

    // rotate the face.
    val t0 = array(9)
    val t1 = array(10)
    val t2 = array(11)
    array(9) = array(27)
    array(10) = array(18)
    array(18) = array(28)
    array(27) = array(29)
    array(28) = array(20)
    array(11) = t0
    array(20) = t1
    array(29) = t2

    new Cube(array.toList, if(trace) moves :+ Move.U.inverse else moves)
  }

  private def doD(trace : Boolean): Cube = {
    val array : Array[Color] = cells.toArray

    val temp0 = array(47)
    val temp1 = array(50)
    val temp2 = array(53)

    // move face 2 to face 3.
    array(47) = array(38)
    array(50) = array(41)
    array(53) = array(44)

    // move face 1 to face 2.
    array(38) = array(14)
    array(41) = array(23)
    array(44) = array(32)

    //move face 0 to face 1.
    array(14) = array(2)
    array(23) = array(5)
    array(32) = array(8)

    // move face 3 to face 0.
    array(2) = temp0
    array(5) = temp1
    array(8) = temp2

    // rotate the face.
    val t0 = array(15)
    val t1 = array(16)
    val t2 = array(17)
    array(16) = array(26)
    array(17) = array(35)
    array(26) = array(34)
    array(35) = array(33)
    array(34) = array(24)
    array(33) = t0
    array(24) = t1
    array(15) = t2

    new Cube(array.toList, if(trace) moves :+ Move.D else moves)
  }

  private def undoD(trace : Boolean) : Cube = {
    val array : Array[Color] = cells.toArray

    val temp0 = array(2)
    val temp1 = array(5)
    val temp2 = array(8)

    // move face 1 to face 0.
    array(2) = array(14)
    array(5) = array(23)
    array(8) = array(32)

    // move face 2 to face 1.
    array(14) = array(38)
    array(23) = array(41)
    array(32) = array(44)

    // move face 3 to face 2.
    array(38) = array(47)
    array(41) = array(50)
    array(44) = array(53)

    // move face 0 to face 3.
    array(47) = temp0
    array(50) = temp1
    array(53) = temp2

    // rotate the face.
    val t0 = array(15)
    val t1 = array(16)
    val t2 = array(17)
    array(16) = array(24)
    array(24) = array(34)
    array(15) = array(33)
    array(34) = array(26)
    array(33) = array(35)
    array(17) = t0
    array(26) = t1
    array(35) = t2

    new Cube(array.toList, if(trace) moves :+ Move.D.inverse else moves)
  }

  private def doF(trace : Boolean) : Cube = {
    val array : Array[Color] = cells.toArray

    val temp0 = array(6)
    val temp1 = array(7)
    val temp2 = array(8)

    // move O to B.
    array(6) = array(15)
    array(7) = array(24)
    array(8) = array(33)

    // move G to O.
    array(15) = array(38)
    array(24) = array(37)
    array(33) = array(36)

    // move R to G.
    array(38) = array(29)
    array(37) = array(20)
    array(36) = array(11)

    // move B to R.
    array(29) = temp0
    array(20) = temp1
    array(11) = temp2

    // rotate the face.
    val t0 = array(12)
    val t1 = array(13)
    val t2 = array(14)
    array(13) = array(23)
    array(14) = array(32)
    array(23) = array(31)
    array(32) = array(30)
    array(31) = array(21)
    array(30) = t0
    array(21) = t1
    array(12) = t2

    new Cube(array.toList, if(trace) moves :+ Move.F else moves)
  }

  private def undoF(trace : Boolean) : Cube = {
    val array : Array[Color] = cells.toArray

    val temp0 = array(36)
    val temp1 = array(37)
    val temp2 = array(38)

    // move O to G.
    array(38) = array(15)
    array(37) = array(24)
    array(36) = array(33)

    // move B to O.
    array(15) = array(6)
    array(24) = array(7)
    array(33) = array(8)

    // move R to B.
    array(8) = array(11)
    array(7) = array(20)
    array(6) = array(29)

    // move G to R.
    array(11) = temp0
    array(20) = temp1
    array(29) = temp2

    // rotate the face.
    val t0 = array(12)
    val t1 = array(13)
    val t2 = array(14)
    array(12) = array(30)
    array(13) = array(21)
    array(31) = array(23)
    array(30) = array(32)
    array(21) = array(31)
    array(14) = t0
    array(23) = t1
    array(32) = t2

    new Cube(array.toList, if(trace) moves :+ Move.F.inverse else moves)
  }

  private def doB(trace : Boolean): Cube = {
    val array : Array[Color] = cells.toArray

    val temp0 = array(0)
    val temp1 = array(1)
    val temp2 = array(2)

    // move R to B.
    array(0) = array(27)
    array(1) = array(18)
    array(2) = array(9)

    // move G to R.
    array(9) = array(42)
    array(18) = array(43)
    array(27) = array(44)

    // move O to G.
    array(42) = array(35)
    array(43) = array(26)
    array(44) = array(17)

    // move B to O.
    array(17) = temp0
    array(26) = temp1
    array(35) = temp2

    // rotate the face.
    val t0 = array(45)
    val t1 = array(46)
    val t2 = array(47)
    array(46) = array(50)
    array(47) = array(53)
    array(50) = array(52)
    array(53) = array(51)
    array(52) = array(48)
    array(51) = t0
    array(48) = t1
    array(45) = t2

    new Cube(array.toList, if(trace) moves :+ Move.B else moves)
  }

  private def undoB(trace : Boolean): Cube = {
    val array : Array[Color] = cells.toArray

    val temp0 = array(0)
    val temp1 = array(1)
    val temp2 = array(2)

    // move O to B.
    array(0) = array(17)
    array(1) = array(26)
    array(2) = array(35)

    // move G to O.
    array(17) = array(44)
    array(26) = array(43)
    array(35) = array(42)

    // move R to G.
    array(42) = array(9)
    array(43) = array(18)
    array(44) = array(27)

    // move B to R.
    array(9) = temp2
    array(18) = temp1
    array(27) = temp0

    // rotate the face.
    val t0 = array(45)
    val t1 = array(46)
    val t2 = array(47)
    array(45) = array(51)
    array(46) = array(48)
    array(48) = array(52)
    array(52) = array(50)
    array(51) = array(53)
    array(47) = t0
    array(50) = t1
    array(53) = t2

    new Cube(array.toList, if(trace) moves :+ Move.B.inverse else moves)
  }

  private def doL(trace : Boolean): Cube = {
    val array : Array[Color] = cells.toArray

    val temp0 = array(51)
    val temp1 = array(52)
    val temp2 = array(53)

    // move O to Y.
    array(51) = array(15)
    array(52) = array(16)
    array(53) = array(17)

    // move W to O.
    array(15) = array(12)
    array(16) = array(13)
    array(17) = array(14)

    // move R to W.
    array(12) = array(9)
    array(13) = array(10)
    array(14) = array(11)

    // move Y to R.
    array(9) = temp2
    array(10) = temp1
    array(11) = temp0

    // rotate the face.
    val t0 = array(0)
    val t1 = array(1)
    val t2 = array(2)
    array(1) = array(5)
    array(2) = array(8)
    array(5) = array(7)
    array(8) = array(6)
    array(7) = array(3)
    array(6) = t0
    array(3) = t1
    array(0) = t2

    new Cube(array.toList, if(trace) moves :+ Move.L else moves)
  }

  private def undoL(trace : Boolean) : Cube = {
    val array : Array[Color] = cells.toArray

    val temp0 = array(9)
    val temp1 = array(10)
    val temp2 = array(11)

    // move W to R.
    array(9) = array(12)
    array(10) = array(13)
    array(11) = array(14)

    // move O to W.
    array(12) = array(15)
    array(13) = array(16)
    array(14) = array(17)

    // move Y to O.
    array(15) = array(53)
    array(16) = array(52)
    array(17) = array(51)

    // move R to Y.
    array(51) = temp2
    array(52) = temp1
    array(53) = temp0

    // rotate the face.
    val t0 = array(0)
    val t1 = array(1)
    array(0) = array(6)
    array(1) = array(3)
    array(3) = array(7)
    array(6) = array(8)
    array(7) = array(5)
    array(8) = array(2)
    array(5) = t1
    array(2) = t0

    new Cube(array.toList, if(trace) moves :+ Move.L.inverse else moves)
  }

  private def doR(trace : Boolean): Cube = {
    val array : Array[Color] = cells.toArray

    val temp0 = array(27)
    val temp1 = array(28)
    val temp2 = array(29)

    // move W to R.
    array(27) = array(30)
    array(28) = array(31)
    array(29) = array(32)

    // move O to W.
    array(30) = array(33)
    array(31) = array(34)
    array(32) = array(35)

    // move Y to O.
    array(33) = array(47)
    array(34) = array(46)
    array(35) = array(45)

    // move R to Y.
    array(45) = temp2
    array(46) = temp1
    array(47) = temp0

    // rotate the face.
    /*

             09 18 27
             10 19 28
             11 20 29
    00 03 06 12 21 30 36 39 42 45 48 51
    01 04 07 13 22 31 37 40 43 46 49 52
    02 05 08 14 23 32 38 41 44 47 50 53
             15 24 33
             16 25 34
             17 26 35


     */
    val t0 = array(36)
    val t1 = array(37)
    val t2 = array(38)
    array(37) = array(41)
    array(38) = array(44)
    array(41) = array(43)
    array(44) = array(42)
    array(43) = array(39)
    array(42) = t0
    array(39) = t1
    array(36) = t2

    new Cube(array.toList, if(trace) moves :+ Move.R else moves)
  }

  private def undoR(trace : Boolean) : Cube = {

    val array : Array[Color] = cells.toArray

    val temp0 = array(45)
    val temp1 = array(46)
    val temp2 = array(47)

    // move O to G.
    array(45) = array(35)
    array(46) = array(34)
    array(47) = array(33)

    // move W to O.
    array(33) = array(30)
    array(34) = array(31)
    array(35) = array(32)

    // move R to W.
    array(30) = array(27)
    array(31) = array(28)
    array(32) = array(29)

    // move Y to R.
    array(27) = temp2
    array(28) = temp1
    array(29) = temp0

    // rotate the face.
    val t0 = array(36)
    val t1 = array(37)
    val t2 = array(38)
    array(36) = array(42)
    array(37) = array(39)
    array(39) = array(43)
    array(43) = array(41)
    array(42) = array(44)
    array(38) = t0
    array(41) = t1
    array(44) = t2


    new Cube(array.toList, if(trace) moves :+ Move.R.inverse else moves)
  }

  override def equals(o : Any) : Boolean = o match {
    case c : Cube =>
      for(i <- 0 to 53) {
        if(c.cells(i) != cells(i)) return false
      }
      true
    case _ => false
  }

  override def hashCode : Int = _hashCode

  override def toString = _str
}

object Cube {
  private val solvedCells = {
    val cells = Array.ofDim[Color] (54)
    for(i <- 0 to 2) {
      cells(i) = Green
      cells(i+3) = Green
      cells(i+6) = Green
    }
    for(i <- 9 to 11) {
      cells(i) = Yellow
      cells(i+9) = Yellow
      cells(i+18) = Yellow
    }
    for(i <- 12 to 14) {
      cells(i) = Orange
      cells(i+9) = Orange
      cells(i+18) = Orange
    }
    for(i <- 15 to 17) {
      cells(i) = White
      cells(i+9) = White
      cells(i+18) = White
    }
    for(i <- 36 to 38) {
      cells(i) = Blue
      cells(i+3) = Blue
      cells(i+6) = Blue
    }
    for(i <- 45 to 47) {
      cells(i) = Red
      cells(i+3) = Red
      cells(i+6) = Red
    }

    cells.toList
  }
  val solved = new Cube(solvedCells)
  def scrambled(n : Int = 100, trace : Boolean = false) : Cube = {
    assert(n >= 0)
    var cube = solved
    for (i <- 1 to n) cube = cube.make(Move.random, trace)
    cube
  }
}