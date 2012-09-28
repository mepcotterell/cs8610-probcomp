package probcomp

/**
 * A disjoint sets data structure.
 *
 * @author Michael E. Cotterell <mepcotterell@gmail.com>
 * @param n the number of initial disjoint sets
 */
class DisjointSets (n: Int) {

  // make sure we're using mutable sets
  import scala.collection.mutable.{ HashSet, Set }

  // create a parallel set of mutable sets
  val sets = Set.empty[Set[Int]]

  // create the initial disjoint sets
  for (i <- 0 until n) {
    sets add Set(i)
  } // for
 
  /**
   * Returns the indices in the set array of the sets that contain x.
   * 
   * @param an element to find
   * @return an array of indices corresponding to the sets containing x
   */
  def find (x: Int): Set[Int] = {
    var set: Set[Int] = null
    sets.view.filter(set => set.exists(_ == x)).foreach(s => set = s)
    set
  } // find

  /**
   * Unites the dynamic sets that contains x and y into a new set that is the
   * union of these two sets.
   *
   * @param x an element of the first set
   * @param y an element of the second set
   */
  def union (x: Int, y: Int): Unit = {

    // get each set
    val xSet = find(x)
    val ySet = find(y)

    // if they're not equal, merge the smaller set into the larger set
    if (xSet != ySet){
      if (xSet.size > ySet.size) {
	xSet ++= ySet
	sets remove ySet
      } else {
	ySet ++= xSet
	sets remove xSet
      } // if
    } // if

  } // union

} // DisjointSets

