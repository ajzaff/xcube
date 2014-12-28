package com.alanjz.xcube.moves

trait Move {
  def inverse : Move
}

trait IdentityMove extends Move {
  override def inverse = this
}