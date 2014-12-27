package com.alanjz.xcube.solver

import com.alanjz.xcube.{Move, Cube}

import scala.collection.mutable

object Solver extends App {
  val cube = Cube.scrambled(3, trace=true)

  println(cube)
  val start = System.nanoTime()
  val solution = solve(cube)
  print(s"solution: ")
  if(solution.isDefined) println(solution.get.mkString(" "))
  else println("None")
  println(s"${(System.nanoTime()-start)/1e9}s")

  def solve(cube : Cube) : Option[List[Move]] = {
    val queue = new mutable.Queue[Cube]()
    val visited = new mutable.HashSet[Int]()
    queue.enqueue(cube)
    visited.add(cube.hashCode)
    while(queue.nonEmpty) {
      val t = queue.dequeue()
      if(t.isSolved) {
        return Some(t.moves)
      }
      for(move <- Move.allMoves) {
        val u = t.make(move)
        if(!visited.contains(u.hashCode)) {
          visited.add(u.hashCode)
          queue.enqueue(u)
        }
      }
    }
    None
  }
}