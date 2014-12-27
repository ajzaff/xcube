package com.alanjz.xcube.solver

import com.alanjz.xcube.{Move, Cube}

object CubeTest extends App {
  var cube = Cube.solved
  val hash = cube.hashCode
  println(cube)

  cube = cube.make(Move.F)
  println(cube)
  cube = cube.unmake(Move.F)
  println(cube)
  assert(cube.hashCode == hash)

  cube = cube.make(Move.R)
  println(cube)
  cube = cube.unmake(Move.R)
  println(cube)
  assert(cube.hashCode == hash)

  cube = cube.make(Move.L)
  println(cube)
  cube = cube.unmake(Move.L)
  println(cube)
  assert(cube.hashCode == hash)

  cube = cube.make(Move.U)
  println(cube)
  cube = cube.unmake(Move.U)
  println(cube)
  assert(cube.hashCode == hash)

  cube = cube.make(Move.D)
  println(cube)
  cube = cube.unmake(Move.D)
  println(cube)
  assert(cube.hashCode == hash)

  cube = cube.make(Move.B)
  println(cube)
  cube = cube.unmake(Move.B)
  println(cube)
  assert(cube.hashCode == hash)
}
