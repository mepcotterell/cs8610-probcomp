package probcomp

import scala.collection.mutable.Set

class DisjointSetList (n: Int) {

  // create an array of mutable sets
  val sets = Array.ofDim[Set[Int]](n)
  
  // create the sets
  for (i <- 0 until sets.length) sets(i) = Set()

  def find (i: Int): Int = {
    for (e <- sets.view.zipWithIndex if e._1.exists(_ == i)) return e._2
    return -1
  } // find

  def untion (i: Int, j: Int) = {}

} // DisjointSetList

object Midterm extends App {


  val dsl = new DisjointSetList(10)

  dsl.find(2)

} // Midterm


