package probcomp.random

import collection.generic.CanBuildFrom

object Implicits {

  implicit def mkRandomRange (r: Range) = new RandomRange(r)

} // Implicits

/**
 * Represents a uniform (0.0, 1.0) random variable
 */
class Random () {

  // random number generator
  private val r = new scala.util.Random()

  /**
   * Returns a value from the codomain of this random variable with uniform
   * probability.
   */
  def gen: Double = r.nextDouble

  def shuffle [T, CC[X] <: TraversableOnce[X]](xs: CC[T])(implicit bf: CanBuildFrom[CC[T], T, CC[T]]) = r.shuffle(xs)

} // Random

/**
 * Provided uniform random ordering to ranges
 */
class RandomRange (r: Range) {

  /**
   * Returns a shuffled version of this Range
   */
  def shuffled = new scala.util.Random().shuffle(r toIterable)

} // RandomRange

