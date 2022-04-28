package week3

import week3.model.Rational

object rationals extends App {
  val x = new Rational(1, 3)
  val y = new Rational(5, 7)
  val z = new Rational(3, 2)

  println(x.numer)
  println(x.denom)
  println(x.add(y))
  println(x.sub(y).sub(z))
}
