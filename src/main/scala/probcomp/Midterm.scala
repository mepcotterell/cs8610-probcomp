package probcomp

import scala.collection.mutable.Set

class DisjointSets (n: Int) {

  // create an array of mutable sets
  val sets = Array.ofDim[Set[Int]](n)
  
  // create the sets
  for (i <- 0 until sets.length) sets(i) = Set(i)

  def find (x: Int): Array[Int] = sets.view.zipWithIndex.filter(e => e._1.exists(_ == x)).map(e => e._2).toArray

  def union (x: Int, y: Int): Unit = for (xRoot <- find(x); yRoot <- find(y)) {
    sets(xRoot) ++= sets(yRoot)
    sets(yRoot)   = sets(xRoot)
  } // union

} // DisjointSets

class DisjointSetGraph (n: Int) extends DisjointSets (n) {

  def addEdge (x: Int, y: Int): Unit = {
    union(x, y)
    println("add (%d, %d) => %s".format(x, y, sets.deep))
  } // addEdge

} // DisjointSetGraph

object Midterm extends App {

  val dsg = new DisjointSetGraph(10)

  println("initial => %s".format(dsg.sets.deep))

  dsg.addEdge(1, 3)
  dsg.addEdge(4, 6)
  dsg.addEdge(0, 2)
  dsg.addEdge(7, 8)
  dsg.addEdge(0, 1)
  dsg.addEdge(4, 5)
  dsg.addEdge(1, 2)

} // Midterm


