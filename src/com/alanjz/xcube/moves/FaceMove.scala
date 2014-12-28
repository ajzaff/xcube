package com.alanjz.xcube.moves

trait FaceMove extends Move {
   override def inverse : FaceMove
}

object FaceIdentityMove extends IdentityMove