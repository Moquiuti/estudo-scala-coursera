package week5

object exercises extends App {

  /**
   * Implement init as an external function, analogous to last
   * @param xs
   * @tparam T
   * @return
   */
  def init[T](xs: List[T]): List[T] = xs match {
    case List() => throw new Error("init of empty list")
    case List(x) => List()
    case y :: ys => y :: init(ys)
  }

  /**
   * Remove the n'th element of a list xs. If n is out of bounds, return xs itself.
   * @param n
   * @param xs
   * @tparam T
   * @return
   */
  def removeAt[T](n: Int, xs: List[T]) = (xs take n) ::: (xs drop n + 1)

  println(removeAt(1, List('a', 'b', 'c', 'd'))) // List(a, c, d)

  /**
   *
   * @param xs
   * @return
   */
  def flatten(xs: List[Any]): List[Any] = xs match {
    case List() => List()
    case (y :: ys) :: yss => flatten(y :: ys) ::: flatten(yss)
    case y :: ys => y :: flatten(ys)
  }

  println(flatten(List(List(1, 1), 2, List(3, List(5, 8))))) // res0: List[Any] = List(1, 1, 2, 3, 5, 8)
}
