package week1

object Session extends App {
  def abs(x: Double) = if (x < 0) -x else x

  def sqrIter(guess: Double, x: Double): Double =
    if (isGoodEnough(guess, x)) guess
    else sqrIter(improve(guess, x), x)

  def isGoodEnough(guess: Double, x: Double) = abs(guess * guess - x) / x < 0.001

  def improve(guess: Double, x: Double) = (guess + x / guess) / 2

  def sqr(x: Double) = sqrIter(1.0, x)

  println(sqr(0.001))
  println(sqr(0.1e-20))
  println(sqr(1.0e20))
  println(sqr(1.0e50))

}
