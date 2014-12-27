package com.alanjz.xcube

import scala.util.Random

sealed abstract class Move(val name : String) {
  def inverse : Move
  override def toString = name
}

object Move {

  val allMoves = List(LPrime, RPrime, UPrime, DPrime, FPrime, BPrime, L, R, U, D, F, B)

  def random : Move = allMoves.toList(Random.nextInt(12))

  case object LPrime extends Move("L'") { override def inverse = L }
  case object RPrime extends Move("R'") { override def inverse = R }
  case object UPrime extends Move("U'") { override def inverse = U }
  case object DPrime extends Move("D'") { override def inverse = D }
  case object FPrime extends Move("F'") { override def inverse = F }
  case object BPrime extends Move("B'") { override def inverse = B }

  case object L extends Move("L") { override def inverse = LPrime }
  case object R extends Move("R") { override def inverse = RPrime }
  case object U extends Move("U") { override def inverse = UPrime }
  case object D extends Move("D") { override def inverse = DPrime }
  case object F extends Move("F") { override def inverse = FPrime }
  case object B extends Move("B") { override def inverse = BPrime }
}