package week5


object exercises8 extends App {

  def factorial(n: Int): Int =
    if (n == 0) 1 // 1st clause
    else n * factorial(n - 1) // 2nd clause

  def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
    case List() => ys
    case x :: xs1 => x :: concat(xs1, ys)
  }

}

