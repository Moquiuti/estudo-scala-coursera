package week5

object exercises7 extends App {

  def sum(xs: List[Int]): Int = xs match {
    case Nil => 0
    case y :: ys => y + sum(ys)
  }

  def sum2(xs: List[Int]) = (0 :: xs) reduceLeft ((x, y) => x + y)

  def product(xs: List[Int]) = (1 :: xs) reduceLeft ((x, y) => x * y)

  def sumReduceLeft(xs: List[Int]) = (0 :: xs) reduceLeft (_ + _)

  def product2ReduceLeft(xs: List[Int]) = (1 :: xs) reduceLeft (_ * _)

  def sumFoldLeft(xs: List[Int]) = (xs foldLeft 0) (_ + _)

  def productFoldLeft(xs: List[Int]) = (xs foldLeft 1) (_ * _)

  def concat[T](xs: List[T], ys: List[T]): List[T] =
    (xs foldRight ys) (_ :: _)

  def reverse[T](xs: List[T]): List[T] =
    (xs foldLeft List[T]()) ((xs, x) => x :: xs)


  /**
   *
   * @param xs
   * @param ys
   * @tparam T
   * @return
   */
  def concat[T](xs: List[T], ys: List[T]): List[T] =
    (xs foldRight ys) (_ :: _)

  /**
   * Complete the following definitions of the basic functions map and length on lists,
   * such that their implementation uses foldRight:
   */

  /**
   *
   * @param xs
   * @param f
   * @tparam T
   * @tparam U
   * @return
   */
  def mapFun[T, U](xs: List[T], f: T => U): List[U] =
    (xs foldRight List[U]()) (???)

  /**
   *
   * @param xs
   * @tparam T
   * @return
   */
  def lengthFun[T](xs: List[T]): Int =
    (xs foldRight 0) (???)
}

