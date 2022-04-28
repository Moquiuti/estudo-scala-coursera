package week2

import math.abs

object exercises extends App {
  def fatorial(n: Int): Int = {
    def loop(acc: Int, n: Int): Int =
      if (n == 0) acc
      else loop(acc * n, n - 1)

    loop(1, n)
  }

  println(fatorial(4))

  /**
   * Função de alta ordem ou de ordem superior.
   * Como blocos de contrução de programas.
   * São essenciais em muitas linguagens de programação funcionais.
   *
   * @param f
   * @param a
   * @param b
   * @return
   */
  def sum(f: Int => Int, a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, f(a) + acc)
    }

    loop(a, 0)
  }

  println(sum(x => x * x, 3, 5))

  /** Currying
   * Write a product function that calculates the product of the values
   * of a function for the points on a given interval.
   */
  def product(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x * y, 1)(a, b)

  println(product(x => x * x)(3, 4))

  /** Currying
   * Write factorial in terms of product.
   * é uma multiplicação de todos os números entre um e m
   */
  def fact(n: Int) = product(x => x)(1, n)

  println(fact(5))

  /** Currying
   * Write a more general function, which generalizes both sum and product.
   */
  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
    if (a > b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))

  /**
   * Write a square root function using `fixedPoint` and `averageDamp`.
   */
  val tolerance = 0.0001
  def isCloseEnough(x: Double, y: Double) =
    abs((x - y) / x) / x < tolerance
    def fixedPoint(f: Double => Double)(firstGuess: Double) = {
      def iterage(guess: Double): Double = {
        val next = f(guess)
        if (isCloseEnough(guess, next)) next
        else iterage(next)
      }
      iterage(firstGuess)
    }
  fixedPoint(x => 1 + x/2)(1)
  def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

  def sqrt(x: Double) =
    fixedPoint(averageDamp(y => x / y))(1)

  println(sqrt(2))

  /**
   *
   */
}
