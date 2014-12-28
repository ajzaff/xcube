package com.alanjz.xcube.cube

import com.alanjz.xcube.cube.colors._
import com.alanjz.xcube.moves.FaceMove

class Cube {
}

/**
 * A cube face.
 *
 * @param color the color of this face.
 * @param facets the movable facets of the cube face.
 *
 */

class Face(val color : Color, val facets : Array[Color] = Array.ofDim(8)) {

  for(i <- 0 to facets.length)
    facets(i) = color

  def updated(us : (Int, Color)*) : Face = {
    val face = new Face(color)
    for (u <- us) {
      face.facets.update(u._1, u._2)
    }
    face
  }
  def turn(m : FaceMove) = {

  }
  def unturn(m : FaceMove) = turn(m.inverse)
}

object Facet {
}