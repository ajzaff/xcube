package com.alanjz.xcube.moves

trait CubeMove extends Move {
   override def inverse : CubeMove
}

object CubeIdentityMove extends IdentityMove