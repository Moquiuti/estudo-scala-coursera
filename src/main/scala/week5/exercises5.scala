package week5

import scala.math.Ordering

object exercises5 extends App {

  /**
   * to multiply each element of a list by the same factor, you could write:
   * @param xs
   * @param factor
   * @return
   */
  def scalelist(xs: List[Double], factor: Double): List[Double] = xs match {
    case Nil => xs
    case y :: ys => y * factor :: scalelist(ys, factor)
  }
}

