package week1

object exercises1 extends App {
  def abs(x: Double) = if (x < 0) -x else x

  def sqr(x: Double) = {
    def sqrIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrIter(improve(guess))

    def isGoodEnough(guess: Double) = abs(guess * guess - x) / x < 0.001

    def improve(guess: Double) = (guess + x / guess) / 2

    println(sqrIter(1.0))
  }

  println(sqr(2))
  println(sqr(4))
  println(sqr(1e-6))
  println(sqr(1e60))
}


