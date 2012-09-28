package probcomp.random

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